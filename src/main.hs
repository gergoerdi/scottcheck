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
import Data.Array
import Options.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

data Mode
    = Solve
    | Play
    deriving (Show, Read, Enum, Bounded)

data Options = Options
    { filePath :: FilePath
    , mode :: Mode
    }

options :: Parser Options
options = do
    mode <- flag Solve Play $ mconcat
        [ long "interactive"
        , short 'i'
        , help "Interactive mode"
        ]
    filePath <- strArgument $ mconcat
        [ metavar "FILENAME"
        ]
    pure Options{..}

optionsInfo = info (options <**> helper) $ mconcat
    [ fullDesc
    , header "ScottCheck"
    , progDesc "SMT based verification of Scott Adams adventure games"
    ]

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

    let resolve (v, n) = (gameVerbsRaw theGame ! v, gameNounsRaw theGame ! n)

    mapM_ (print . resolve) cmds

play :: Game -> IO ()
play theGame = runSMT $ do
    arr <- newArray "items" Nothing
    query $ go $ initState theGame arr
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

        ensureSat
        finished <- getValue finished
        output <- mapM getValue output
        liftIO $ mapM_ putStrLn output
        case finished of
            Just won -> finishWith won
            Nothing -> return s'

    round s = do
        s <- io s stepWorld
        (verb, noun) <- liftIO input
        s <- io s $ stepPlayer (literal verb, literal noun)
        return s

    finishWith won = liftIO $ do
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

    case mode of
        Solve -> solve theGame
        Play -> play theGame
