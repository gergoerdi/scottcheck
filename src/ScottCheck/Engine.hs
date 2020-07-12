{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine (Game(..), Item(..), initState, runGame, stepPlayer) where

import ScottCheck.Utils

import Control.Monad.Reader
import Control.Monad.State
import Data.SBV.MTL ()

import Control.Lens
import Data.Int
import Data.Array as A

import GHC.Generics (Generic)
import Data.SBV
import qualified Data.SBV.Maybe as SBV

data Game = Game
    { gameStartRoom :: Int16
    , gameTreasury :: Int16
    , gameMaxScore :: Int16
    , gameDictSize :: Int16
    , gameItems :: Array Int16 Item
    , gameRooms :: Array Int16 [Int16]
    }
    deriving (Show)

data Item = Item Bool (Maybe Int16) Int16 deriving (Show)


type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _itemLocations :: SFunArray Int16 Int16
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (State S)

carried :: Int16
carried = 255

fillArray :: (Ix a, SymArray sarr, SymVal a, SymVal b) => Array a b -> sarr a b -> sarr a b
fillArray arr sarr = foldr write sarr (A.assocs arr)
  where
    write (i, x) sarr = writeArray sarr (literal i) (literal x)

initState :: Game -> SFunArray Int16 Int16 -> S
initState game itemsArr = S
    { _currentRoom = literal $ gameStartRoom game
    , _itemLocations = fillArray (fmap (\(Item _ _ loc) -> loc) $ gameItems game) itemsArr
    }

runGame :: Game -> Engine a -> State S a
runGame game act = runReaderT act game

stepPlayer :: SInput -> Engine SBool
stepPlayer (v, n) = do
    perform (v, n)
    finished

finished :: Engine SBool
finished = do
    maxScore <- asks gameMaxScore
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocs <- use itemLocations
    let treasureLocs = [ readArray itemLocs (literal item) | (item, Item True _ _) <- A.assocs items ]
    let haveAllTreasure = map (.== literal treasury) treasureLocs `pbAtLeast` fromIntegral maxScore

    return haveAllTreasure

builtin :: SInput -> Engine ()
builtin (verb, noun) = sCase verb (return ())
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
  where
    builtin_go = sWhen (1 .<= noun .&& noun .<= 6) $ do
        let dir = noun - 1
        here <- use currentRoom
        exits <- asks $ (.! here) . fmap (map literal) . gameRooms
        let newRoom = select exits 0 dir
        sWhen (newRoom ./= 0) $ currentRoom .= newRoom

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        sWhen (readArray locs item .== here) $ move item (literal carried)

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        sWhen (readArray locs item .== literal carried) $ move item here

    parseItem = do
        items <- asks gameItems
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items


perform :: SInput -> Engine ()
perform (verb, noun) = do
    builtin (verb, noun)
    return ()

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
