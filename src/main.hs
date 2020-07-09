{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO
import System.Exit

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine
import ScottCheck.Utils

import qualified Data.Map as M
import Data.SBV hiding (options)
import Data.SBV.Control
import Data.SBV.Internals (SMTModel(..), CV(..), CVal(..))
import qualified Data.SBV.Maybe as SBV
import qualified Data.SBV.Tuple as SBV
import Debug.Trace
import Text.Printf
import Data.List (stripPrefix, sortBy)
import Data.Ord (comparing)
import Data.Either
import Data.Array
import Options.Applicative
import Control.Monad

data Options = Options
    { filePath :: FilePath
    }

options :: Parser Options
options = do
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

    hSetBuffering stdout NoBuffering

    s <- T.readFile filePath
    let Right theGame = parseOnly game s
    -- print theGame

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

    let resolve (v, n) = (gameVerbsRaw theGame ! v, gameNounsRaw theGame ! n)

    mapM_ (print . resolve) cmds
