module Main where

import ScottCheck.Engine

import Data.SBV hiding (options, solve)
import Data.SBV.Control
import qualified Data.Array as A
import Control.Monad.IO.Class

solve :: Game -> IO ()
solve theGame = do
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ do
            s <- return $ initState theGame arr

            verb1 <- genWord
            noun1 <- genWord
            push 1
            (finished, s) <- return $ step theGame (verb1, noun1) s
            constrain finished
            cs <- checkSat
            liftIO $ print cs

            pop 1
            verb2 <- genWord
            noun2 <- genWord
            push 1
            (finished, s) <- return $ step theGame (verb2, noun2) s
            constrain finished
            cs <- checkSat
            liftIO $ print cs

            mapM getValue [verb1, noun1, verb2, noun2]
    mapM_ print cmds
  where
    genWord = do
        word <- freshVar_
        constrain $ 0 .<= word .&& word .< literal (gameDictSize theGame)
        return word

main :: IO ()
main = do
    let theGame = Game
            { gameStartRoom = 1
            , gameTreasury = 1
            , gameDictSize = 19
            , gameItems = A.listArray (0,0)
                  [ Item (Just 7) 255
                  ]
            }

    solve theGame
