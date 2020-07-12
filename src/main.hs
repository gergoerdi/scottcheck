module Main where

import ScottCheck.Engine

import Data.SBV hiding (options, solve)
import Data.SBV.Control
import qualified Data.Array as A
import Control.Monad.IO.Class

main :: IO ()
main = do
    let theGame = A.listArray (0,0) [ Item (Just 7) 255 ]
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ do
            s <- return $ initState arr

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
        constrain $ 0 .<= word .&& word .< literal 19
        return word
