{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine (Game(..), Item(..), initState, stepPlayer) where

import ScottCheck.Utils

import Data.Int
import Data.Array as A

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

type SArr = SFunArray

type S = (SInt16, SArr Int16 Int16)

carried :: Int16
carried = 255

fillArray :: (Ix a, SymArray sarr, SymVal a, SymVal b) => Array a b -> sarr a b -> sarr a b
fillArray arr sarr = foldr write sarr (A.assocs arr)
  where
    write (i, x) sarr = writeArray sarr (literal i) (literal x)

initState :: Game -> SArr Int16 Int16 -> S
initState Game{..} itemsArr = (literal gameStartRoom, fillArray (fmap (\(Item _ _ loc) -> loc) gameItems) itemsArr)

stepPlayer :: Game -> SInput -> S -> (SBool, S)
stepPlayer game (verb, noun) s =
    let s' = builtin game (verb, noun) s
    in (finished game s', s')

finished :: Game -> S -> SBool
finished Game{..} (_, itemLocs) = readArray itemLocs (literal 0) .== literal gameTreasury

builtin :: Game -> SInput -> S -> S
builtin Game{..} (verb, noun) s = sCase verb s
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
  where
    builtin_go = ite (sNot $ 1 .<= noun .&& noun .<= 6) s $
        let dir = noun - 1
            (here, locs) = s
            exits = (map literal <$> gameRooms) .! here
            newRoom = select exits 0 dir
        in ite (newRoom .== 0) s (newRoom, locs)

    builtin_get =
        let (here, locs) = s
            item = parseItem
        in ite (readArray locs item ./= here) s $ move item (literal carried)

    builtin_drop =
        let (here, locs) = s
            item = parseItem
        in ite (readArray locs item ./= literal carried) s $ move item here

    parseItem = SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems gameItems

    move :: SInt16 -> SInt16 -> S
    move item loc = let (here, locs) = s
                    in (here, writeArray locs item loc)
