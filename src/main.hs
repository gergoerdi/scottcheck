{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine

import Control.Monad.Writer
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

    let play cmds = runWriter $ runGame theGame $ do
            forM_ cmds $ \(v, n) -> do
                end1 <- stepWorld
                ite (SBV.isJust end1) (return end1) $ stepPlayer (v, n)
            finished

    SatResult r <- sat $ do
        cmds <- forM [1..inputLength] $ \i -> do
            let word name = do
                    var <- free $ printf "%s@%d" name (i :: Int)
                    constrain $ 0 .<= var .&& var .< literal (gameDictSize theGame)
                    return var
            v <- word "verb"
            n <- word "noun"
            return (v, n)
        return $ SBV.fromMaybe sFalse $ fst $ play cmds

    let solution = case r of
            Unsatisfiable _ _ -> error "Cannot solve"
            Satisfiable _config SMTModel{..} -> zip
                                                (map (resolve gameVerbsRaw . snd) . sortBy (comparing fst) $ verbs)
                                                (map (resolve gameNounsRaw . snd) . sortBy (comparing fst) $ nouns)
              where
                assocs =
                    [ (either (Left . (,val)) (Right . (,val)) what)
                    | (name, CV _ (CInteger val)) <- modelAssocs
                    , Just what <- return $ classify name
                    ]
                (verbs, nouns) = partitionEithers assocs
                resolve arr w = arr theGame ! fromIntegral w
          where
            classify s = msum
                [ Left . read @Int <$> stripPrefix "verb@" s
                , Right . read @Int <$> stripPrefix "noun@" s
                ]

    forM_ solution $ \(w1, w2) -> printf "> %s %s\n" w1 w2
