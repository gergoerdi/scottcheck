{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import ScottCheck.Engine
import ScottCheck.Utils (loopState)

import Data.SBV hiding (options, solve)
import Data.SBV.Control
import qualified Data.SBV.Tuple as SBV
import qualified Data.Array as A

solve :: Game -> IO ()
solve theGame = do
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ loopState genInput (initState theGame arr) $ \cmd -> do
            let (verb, noun) = SBV.untuple cmd
            (finished, output) <- runGame theGame $ stepPlayer (verb, noun)
            return finished

    mapM_ print cmds
  where
    genWord = do
        word <- freshVar_
        constrain $ 0 .<= word .&& word .< literal (gameDictSize theGame)
        return word
    genInput i = do
        verb <- genWord
        noun <- genWord
        return $ SBV.tuple (verb, noun)

main :: IO ()
main = do
    let theGame = Game
            { gameStartRoom = 1
            , gameTreasury = 2
            , gameMaxScore = 1
            , gameDictSize = 19
            , gameItems = A.array (0,1) [(0,Item False Nothing "Sign says: leave treasure here, then say SCORE" 2),(1,Item True (Just 7) "*Gold coin*" 3)]
            , gameRooms = A.array (0,3) [(0,Room [0,0,0,0,0,0] ""),(1,Room [0,2,0,0,0,0] "gorgeously decorated throne room"),(2,Room [1,3,0,0,0,0] "N-S corridor"),(3,Room [2,0,0,0,0,0] "square chamber")]
            }

    solve theGame
