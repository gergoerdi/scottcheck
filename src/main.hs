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
import Control.Monad.State
import Control.Monad.IO.Class

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

input :: Game -> IO (Int16, Int16)
input theGame = do
    putStr "> "
    line <- getLine
    let (w1, w2) = case words line of
            (w1:w2:_) -> (w1, w2)
            [w1] -> (w1, "")
            [] -> ("", "")
    maybe (putStrLn "I didn't get that." >> input theGame) return $ parseInput theGame w1 w2

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    hSetBuffering stdout NoBuffering

    s <- T.readFile filePath
    let Right theGame = parseOnly game s
    -- print theGame

    runSMT $ query $ do
        let round s = do
                ((finished, output), s) <- return $ flip runState s $ runGame theGame $ stepWorld

                ensureSat
                finished <- getValue finished
                output <- mapM getValue output
                liftIO $ mapM_ putStrLn output
                -- TODO: finished?

                (verb, noun) <- liftIO $ input theGame
                ((finished, output), s) <- return $ flip runState s $ runGame theGame $ stepPlayer (literal verb, literal noun)

                ensureSat
                finished <- getValue finished
                output <- mapM getValue output
                liftIO $ mapM_ putStrLn output
                -- TODO: finished?

                return s

        let loop s = do
                round s >>= loop
        loop $ initState theGame
