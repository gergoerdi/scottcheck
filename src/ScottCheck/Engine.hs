{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module ScottCheck.Engine (Game(..), Room(..), Item(..), initState, runGame, stepWorld, stepPlayer) where

import ScottCheck.Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.SBV.MTL ()

import Control.Lens
import Data.Int
import Data.Array as A

import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Maybe as SBV

data Game = Game
    { gameStartRoom :: Int16
    , gameTreasury :: Int16
    , gameMaxScore :: Int16
    , gameDictSize :: Int16
    , gameItems :: Array Int16 Item
    , gameRooms :: Array Int16 Room
    }
    deriving (Show)

data Room = Room [Int16] String deriving (Show)

data Item = Item Bool (Maybe Int16) String Int16 deriving (Show)


type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _itemLocations :: SFunArray Int16 Int16
    , _dead :: SBool
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (WriterT [SString] (State S))

carried :: Int16
carried = 255

fillArray :: (Ix a, SymArray sarr, SymVal a, SymVal b) => Array a b -> sarr a b -> sarr a b
fillArray arr sarr = foldr write sarr (A.assocs arr)
  where
    write (i, x) sarr = writeArray sarr (literal i) (literal x)

initState :: Game -> SFunArray Int16 Int16 -> S
initState game itemsArr = S
    { _currentRoom = literal $ gameStartRoom game
    , _itemLocations = fillArray (fmap (\(Item _ _ _ loc) -> loc) $ gameItems game) itemsArr
    , _dead = sFalse
    }

runGame :: Game -> Engine a -> State S (a, [SString])
runGame game act = runWriterT $ runReaderT act game

stepWorld :: Engine (SMaybe Bool)
stepWorld = do
    perform (0, 0)
    finished

stepPlayer :: SInput -> Engine (SMaybe Bool)
stepPlayer (v, n) = do
    perform (v, n)
    finished

data SRoom = SRoom [SInt16] SString deriving (Show, Generic, Mergeable)

sRoom :: Room -> SRoom
sRoom (Room exits desc) = SRoom (map literal exits) (literal desc)

finished :: Engine (SMaybe Bool)
finished = do
    dead <- use dead

    maxScore <- asks gameMaxScore
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocs <- use itemLocations
    let treasureLocs = [ readArray itemLocs (literal item) | (item, Item True _ _ _) <- A.assocs items ]
    let haveAllTreasure = map (.== literal treasury) treasureLocs `pbAtLeast` fromIntegral maxScore

    return $ ite dead (sJust sFalse) $
      ite haveAllTreasure (sJust sTrue) $
      sNothing

-- sWhen b act = ite b (return ()) act

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
        SRoom exits _ <- asks $ (.! here) . fmap sRoom . gameRooms
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
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items


perform :: SInput -> Engine ()
perform (verb, noun) = do
    builtin (verb, noun)
    return ()

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
