{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, TypeApplications #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import System.IO
-- import Data.Char (toUpper)
-- import Data.Maybe (fromJust)

import ScottCheck.GameData
import ScottCheck.Parse
import ScottCheck.Engine

-- import Control.Monad.State
-- import Control.Monad.Writer
-- import qualified Data.Map as M
-- import Data.SBV
-- import Data.SBV.Maybe (sJust, sNothing)
-- import Data.SBV.Tuple (tuple)
-- import Debug.Trace

-- newtype Transcript i o a = Transcript{ runTranscript :: WriterT [o] (State [i]) a }
--     deriving (Functor, Applicative, Monad, Mergeable)

-- -- instance MonadInteractive (Transcript (String, String) String) where
-- --     emit = Transcript . tell . (:[])
-- --     input = do
-- --         ~(x:xs) <- Transcript get
-- --         Transcript $ put xs
-- --         return x

-- instance MonadInteractive (Transcript (SInt16, SInt16) SString) where
--     emit s = traceShow s $ return () -- Transcript . tell . (:[]) $ s
--     input verbs nouns = do
--         xs <- Transcript get
--         case xs of
--             [] -> return sNothing
--             (x:xs) -> do
--                 Transcript $ put xs
--                 return $ sJust $ tuple x

main :: IO ()
main = do
    s <- T.readFile "t0.sao"
    let Right theGame = parseOnly game s
    print theGame

    let cmds =
            [ ("south", "")
            , ("get", "coin")
            , ("north", "")
            , ("drop", "coin")
            ]
    hSetBuffering stdout NoBuffering
    won <- runGame theGame

    print won
