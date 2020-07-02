{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine

import Control.Monad.Writer
import qualified Data.Map as M
import Data.SBV
import Data.SBV.Internals (SMTModel(..), CV(..), CVal(..))
import qualified Data.SBV.Maybe as SBV
import Debug.Trace
import Text.Printf
import Data.List (stripPrefix, sortBy)
import Data.Ord (comparing)
import Data.Either
import Data.Array

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    s <- T.readFile "t0.sao"
    let Right theGame = parseOnly game s
    -- print theGame

    let play cmds = runWriter $ runGame theGame $ do
            forM_ cmds $ \(v, n) -> do
                end1 <- stepWorld
                ite (SBV.isJust end1) (return end1) $ stepPlayer (v, n)
            finished

    SatResult r <- sat $ do
        cmds <- forM (take 4 [0..]) $ \i -> do
            let word name = do
                    var <- free $ printf "%s@%d" name (i :: Int)
                    constrain $ 0 .<= var .&& var .< literal (gameDictSize theGame)
                    return var
            v <- word "verb"
            n <- word "noun"
            return (v, n)
        return $ SBV.fromMaybe sFalse $ fst $ play cmds

    let solution = case r of
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

    print solution

    let inputs =
            [ ("south", "")
            , ("get", "any")
            , ("north", "")
            , ("drop", "coin")
            ]
        cmds = [ (literal v, literal n)
               | (w1, w2) <- inputs, Just (v, n) <- return $ parseInput theGame w1 w2
               ]

    let cmds =
            [ (literal 1, literal 2)
            , (literal 10, literal 7)
            , (literal 1, literal 1)
            , (literal 18, literal 7)
            ]

    let (endResult, msgs) = play cmds
    mapM_ print msgs
    print endResult
