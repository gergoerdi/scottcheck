{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module ScottCheck.Engine (initState, runGame, stepPlayer) where

import ScottCheck.Utils (sCase, (.!), sFindIndex)

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
    , gameWordLength :: Int16
    , gameDictSize :: Int16
    , gameItems :: Array Int16 Item
    , gameRooms :: Array Int16 Room
    }
    deriving (Show)

data Room = Room [Int16] String deriving (Show)

data Item = Item Bool (Maybe Int16) String Int16 deriving (Show)

theGame = Game
    { gameStartRoom = 1
    , gameTreasury = 1
    , gameMaxScore = 1
    , gameWordLength = 3
    , gameDictSize = 19
    , gameItems = A.listArray (0,1)
        [ Item False Nothing "Sign says: leave treasure here, then say SCORE" 2
        , Item True (Just 7) "*Gold coin*" 3
        ]
    , gameRooms = A.listArray (0,3)
        [ Room [0,0,0,0,0,0] ""
        , Room [0,2,0,0,0,0] "gorgeously decorated throne room"
        , Room [1,3,0,0,0,0] "N-S corridor"
        , Room [2,0,0,0,0,0] "square chamber"
        ]
    }

type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _itemLocations :: SFunArray Int16 Int16
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (WriterT [SString] (State S))

carried :: Int16
carried = 255

say :: SString -> Engine ()
say = tell . (:[])

say_ :: String -> Engine ()
say_ = say . literal

fillArray :: (Ix a, SymArray sarr, SymVal a, SymVal b) => Array a b -> sarr a b -> sarr a b
fillArray arr sarr = foldr write sarr (A.assocs arr)
  where
    write (i, x) sarr = writeArray sarr (literal i) (literal x)

initState :: SFunArray Int16 Int16 -> S
initState itemsArr = S
    { _currentRoom = literal $ gameStartRoom theGame
    , _itemLocations = fillArray (fmap (\(Item _ _ _ loc) -> loc) $ gameItems theGame) itemsArr
    }

runGame :: Engine a -> State S (a, [SString])
runGame act = runWriterT $ runReaderT act theGame

stepPlayer :: SInput -> Engine (SMaybe Bool)
stepPlayer (v, n) = do
    perform (v, n)
    finished

data SRoom = SRoom [SInt16] SString deriving (Show, Generic, Mergeable)

sRoom :: Room -> SRoom
sRoom (Room exits desc) = SRoom (map literal exits) (literal desc)

finished :: Engine (SMaybe Bool)
finished = do
    maxScore <- asks gameMaxScore
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocs <- use itemLocations
    let treasureLocs = [ readArray itemLocs (literal item) | (item, Item True _ _ _) <- A.assocs items ]
    let haveAllTreasure = map (.== literal treasury) treasureLocs `pbAtLeast` fromIntegral maxScore

    return $ ite haveAllTreasure (sJust sTrue) $
      sNothing

builtin :: SInput -> Engine SBool
builtin (verb, noun) = sCase verb (return sFalse)
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
  where
    builtin_go = ite (sNot $ 1 .<= noun .&& noun .<= 6) badDir $ do
        let dir = noun - 1
        here <- use currentRoom
        SRoom exits _ <- asks $ (.! here) . fmap sRoom . gameRooms
        let newRoom = select exits 0 dir
        ite (newRoom .== 0) blocked $ do
            currentRoom .= newRoom
        return sTrue
      where
        badDir = do
            say_ "I don't know how to go in that direction"
            return sTrue

        blocked = say_ "I can't go in that direction."

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (readArray locs item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."
        return sTrue

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (readArray locs item ./= literal carried) (say_ "It's beyond my power to do that.") $ do
            move item here
            say_ "OK."
        return sTrue

    parseItem = do
        items <- asks gameItems
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items


perform :: SInput -> Engine ()
perform (verb, noun) = do
    builtin (verb, noun)
    return ()

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
