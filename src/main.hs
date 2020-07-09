{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import System.IO

import ScottCheck.Engine
import ScottCheck.Utils (loopState)

import Data.SBV hiding (options)
import Data.SBV.Control
import qualified Data.SBV.Maybe as SBV
import qualified Data.SBV.Tuple as SBV

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    let genWord = do
            word <- freshVar_
            constrain $ 0 .<= word .&& word .< 19
            return word
        genInput i = curry SBV.tuple <$> genWord <*> genWord

    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ loopState genInput (initState arr) $ \cmd -> do
            let (verb, noun) = SBV.untuple cmd
            (finished, output) <- runGame $ do
                end2 <- stepPlayer (verb, noun)
                ite (SBV.isJust end2) (return $ SBV.fromJust end2) (return sFalse)
            return finished

    mapM_ print cmds
