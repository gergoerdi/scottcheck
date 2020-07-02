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
import qualified Data.SBV.Maybe as SBV
import Debug.Trace

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

    let keyOf dict name = do
            var <- free name
            constrain $ 0 .<= var .&& var .< literal (fromIntegral (M.size dict))
            return var

    r <- satWith z3 $ do
        cmds <- replicateM 4 $ do
            v <- keyOf (gameVerbs theGame) "verb"
            n <- keyOf (gameNouns theGame) "noun"
            return (v, n)
        return $ SBV.fromMaybe sFalse $ fst $ play cmds
    print r

    let inputs =
            [ ("south", "")
            , ("get", "coin")
            , ("north", "")
            , ("drop", "coin")
            ]
        cmds = [ (literal v, literal n)
               | (w1, w2) <- inputs, Just (v, n) <- return $ parseInput theGame w1 w2
               ]

    -- let cmds =
    --         [ (literal 1, literal 2)
    --         , (literal 10, literal 7)
    --         , (literal 1, literal 1)
    --         , (literal 18, literal 7)
    --         ]

    print $ SBV.fromMaybe sFalse $ fst $ play cmds

    -- let (endResult, msgs) = play cmds
    -- mapM_ print msgs
    -- print endResult
