{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine (Game(..), Item(..), initState, stepPlayer) where

import ScottCheck.Utils

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

stepPlayer :: Game -> SInput -> State S SBool
stepPlayer game (verb, noun) = do
    builtin game (verb, noun)
    finished game

finished :: Game -> State S SBool
finished Game{..} = do
    itemLocs <- use itemLocations
    return $ readArray itemLocs (literal 0) .== literal gameTreasury

builtin :: Game -> SInput -> State S ()
builtin Game{..} (verb, noun) = sCase verb (return ())
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
  where
    builtin_go = sWhen (1 .<= noun .&& noun .<= 6) $ do
        let dir = noun - 1
        here <- use currentRoom
        let exits = (map literal <$> gameRooms) .! here
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
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems gameItems


move :: SInt16 -> SInt16 -> State S ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
