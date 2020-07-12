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
            runGame theGame $ stepPlayer (verb, noun)

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
            , gameItems = A.listArray (0,0)
                  [ Item True (Just 7) 3
                  ]
            , gameRooms = A.listArray (0,3)
                  [ Room [0,0,0,0,0,0]
                  , Room [0,2,0,0,0,0]
                  , Room [1,3,0,0,0,0]
                  , Room [2,0,0,0,0,0]
                  ]
            }

    solve theGame
