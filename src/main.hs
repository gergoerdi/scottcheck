{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO
import System.Exit

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine

import qualified Data.Map as M
import Data.SBV hiding (options)
import Data.SBV.Internals (SMTModel(..), CV(..), CVal(..))
import qualified Data.SBV.Maybe as SBV
import Debug.Trace
import Text.Printf
import Data.List (stripPrefix, sortBy)
import Data.Ord (comparing)
import Data.Either
import Data.Array
import Options.Applicative
import Control.Monad

data Options = Options
    { filePath :: FilePath
    , inputLength :: Int
    }

options :: Parser Options
options = do
    inputLength <- option auto $ mconcat
        [ long "input-length"
        , short 'n'
        , metavar "LENGTH"
        , help "How many commands to input"
        ]
    filePath <- strArgument $ mconcat
        [ metavar "FILENAME"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "ScottCheck"
    , progDesc "SMT based verification of Scott Adams adventure games"
    ]

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    -- hSetBuffering stdout NoBuffering

    s <- T.readFile filePath
    let Right theGame = parseOnly game s
    -- print theGame

    let play = runGame theGame . go
          where
            go [] = finished
            go ((v, n):cmds) = do
                end1 <- stepWorld
                end2 <- ite (SBV.isJust end1) (return end1) $ stepPlayer (v, n)
                ite (SBV.isJust end2) (return end2) $ go cmds

    result <- satWith z3{ verbose = True } $ do
        cmds <- forM [1..inputLength] $ \i -> do
            let word name = do
                    var <- free $ printf "%s@%d" name (i :: Int)
                    constrain $ 0 .<= var .&& var .< literal (gameDictSize theGame)
                    return var
            v <- word "verb"
            n <- word "noun"
            return (v, n)
        return $ SBV.fromMaybe sFalse $ fst $ play cmds

    solution <- case result of
        SatResult (Satisfiable _config SMTModel{..}) -> return $ zip (ordered verbs) (ordered nouns)
          where
            assocs =
                [ (either (Left . (,val)) (Right . (,val)) what)
                | (name, CV _ (CInteger val0)) <- modelAssocs
                , Just what <- return $ classify name
                , let val = fromInteger val0
                ]

            (verbs, nouns) = partitionEithers assocs
            ordered = map snd . sortBy (comparing fst)

            classify s = msum
                [ Left . read @Int <$> stripPrefix "verb@" s
                , Right . read @Int <$> stripPrefix "noun@" s
                ]
        _ -> do
            print result
            exitWith (ExitFailure 1)

    let resolve (v, n) = (gameVerbsRaw theGame ! v, gameNounsRaw theGame ! n)
    forM_ (map resolve solution) $ \(w1, w2) -> printf "> %s %s\n" w1 w2
