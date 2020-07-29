{-# LANGUAGE RecordWildCards, TypeApplications, TupleSections, ApplicativeDo #-}
module Main where

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO
import System.Exit
import Options.Applicative
import Control.Monad.State

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

play :: Game -> IO ()
play theGame = go $ initState theGame
  where
    input = do
        putStr "> "
        line <- getLine
        let (w1, w2) = case words line of
                (w1:w2:_) -> (w1, w2)
                [w1] -> (w1, "")
                [] -> ("", "")
        maybe (putStrLn "I didn't get that." >> input) return $ parseInput theGame w1 w2

    go s = round s >>= go

    io s act = do
        ((finished, output), s') <- return $ flip runState s $ runGame theGame act

        mapM_ putStrLn output
        case finished of
            Just won -> finishWith won
            Nothing -> return s'

    round s = do
        s <- io s stepWorld
        (verb, noun) <- input
        s <- io s $ stepPlayer (verb, noun)
        return s

    finishWith won = do
        putStrLn msg
        exitSuccess
      where
        msg = if won then "You have won!" else "You have died."

main :: IO ()
main = do
    Options{..} <- execParser optionsInfo

    hSetBuffering stdout NoBuffering

    s <- T.readFile filePath
    let Right theGame = parseOnly game s
    -- print theGame

    play theGame
