{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module ScottCheck.Engine where

import ScottCheck.GameData
import ScottCheck.Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.SBV.MTL ()

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Data.List (elemIndex, intercalate)
import Data.Char (toUpper)
import Data.Maybe
import Data.Int
import Text.Printf
import qualified Data.Map as M
import Data.Array as A
import Data.Bifunctor
import Data.Either (partitionEithers)

import Debug.Trace

import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Maybe as SBV

type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _itemLocations :: Array Int16 SInt16
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (State S)

carried :: Int16
carried = 255

say_ :: String -> Engine ()
say_ _ = return ()

initState :: Game -> S
initState game = S
    { _currentRoom = literal $ gameStartRoom game
    , _itemLocations = fmap (\(Item _ _ _ loc) -> literal loc) $ gameItems game
    }

runGame :: Game -> Engine a -> State S a
runGame game act = runReaderT act game

stepWorld :: Engine SBool
stepWorld = do
    perform (0, 0)
    finished

stepPlayer :: SInput -> Engine SBool
stepPlayer (v, n) = do
    perform (v, n)
    finished

data SRoom = SRoom [SInt16] deriving (Show, Generic, Mergeable)

sRoom :: Room -> SRoom
sRoom (Room exits desc) = SRoom (map literal exits)

finished :: Engine SBool
finished = do
    maxScore <- asks gameMaxScore
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocations <- use itemLocations
    let treasureLocs = [ loc | (loc, Item True _ _ _) <- zip (A.elems itemLocations) (A.elems items) ]
    let haveAllTreasure = map (.== literal treasury) treasureLocs `pbExactly` fromIntegral maxScore

    return haveAllTreasure

builtin :: SInput -> Engine SBool
builtin (verb, noun) = sCase verb (return sFalse)
    [ (1, builtin_go)
    , (10, builtin_get)
    ]
  where
    builtin_go = ite (sNot $ 1 .<= noun .&& noun .<= 6) badDir $ do
        let dir = noun - 1
        here <- use currentRoom
        SRoom exits <- asks $ (.! here) . fmap sRoom . gameRooms
        let newRoom = select exits 0 dir
        ite (newRoom .== 0) blocked $ do
            currentRoom .= newRoom
        return sTrue
      where
        badDir = do
            say_ "I don't know how to go in that direction"
            return sTrue

        blocked = return ()

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items
        ite (select (A.elems locs) (-1) item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."
        return sTrue

perform :: SInput -> Engine ()
perform (verb, noun) = do
    builtin (verb, noun)
    return ()

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= replaceAt item loc
