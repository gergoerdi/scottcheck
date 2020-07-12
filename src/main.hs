{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import System.IO
import System.Exit

import ScottCheck.GameData
import ScottCheck.Engine
import ScottCheck.Utils

import qualified Data.Map as M
import Data.SBV hiding (options, solve)
import Data.SBV.Control
import Data.SBV.Internals (SMTModel(..), CV(..), CVal(..))
import qualified Data.SBV.Maybe as SBV
import qualified Data.SBV.Tuple as SBV
import Debug.Trace
import Text.Printf
import Data.List (stripPrefix, sortBy)
import Data.Ord (comparing)
import Data.Either
import qualified Data.Array as A
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

solve :: Game -> IO ()
solve theGame = do
    let genWord name i = do
            word <- freshVar $ printf "%s@%d" name (i :: Int)
            constrain $ 0 .<= word .&& word .< literal (gameDictSize theGame)
            return word
        genInput i = do
            verb <- genWord "verb" i
            noun <- genWord "noun" i
            return $ SBV.tuple (verb, noun)
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ loopState genInput (initState theGame arr) $ \cmd -> do
            let (verb, noun) = SBV.untuple cmd
            (finished, output) <- runGame theGame $ do
                end1 <- stepWorld
                ite (SBV.isJust end1) (return $ SBV.fromJust end1) $ do
                    end2 <- stepPlayer (verb, noun)
                    ite (SBV.isJust end2) (return $ SBV.fromJust end2) (return sFalse)
            return finished

    mapM_ print cmds

main :: IO ()
main = do
    let theGame = Game
            { gameStartRoom = 1
            , gameTreasury = 2
            , gameMaxScore = 1
            , gameDictSize = 19
            , gameItems = A.array (0,1) [(0,Item False Nothing "Sign says: leave treasure here, then say SCORE" 2),(1,Item True (Just 7) "*Gold coin*" 3)]
            , gameActions = [Action (2,0) [(0,0),(0,0),(0,0),(0,0),(0,0)] [65,0]]
            , gameRooms = A.array (0,3) [(0,Room [0,0,0,0,0,0] ""),(1,Room [0,2,0,0,0,0] "gorgeously decorated throne room"),(2,Room [1,3,0,0,0,0] "N-S corridor"),(3,Room [2,0,0,0,0,0] "square chamber")]
            , gameMessages = A.array (0,0) [(0,"")]
            }

    solve theGame
