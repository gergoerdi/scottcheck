module Main where

import ScottCheck.Engine

import Data.SBV hiding (options, solve)
import Data.SBV.Control
import qualified Data.Array as A

main :: IO ()
main = do
    let theGame = A.listArray (0,0) [ Item (Just 7) 255 ]
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ do
            s <- return $ initState arr

            verb1 <- genWord
            noun1 <- genWord
            constrain $ step theGame (verb1, noun1) s
            ensureSat

            mapM getValue [verb1, noun1]
    mapM_ print cmds
  where
    genWord = do
        word <- freshVar_
        constrain $ 0 .<= word .&& word .< literal 19
        return word
