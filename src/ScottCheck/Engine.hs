{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine (Game(..), Item(..), initState, step) where

import Data.Int
import Data.Array as A

import Data.SBV
import qualified Data.SBV.Maybe as SBV

data Game = Game
    { gameStartRoom :: Int16
    , gameTreasury :: Int16
    , gameDictSize :: Int16
    , gameItems :: Array Int16 Item
    }
    deriving (Show)

data Item = Item (Maybe Int16) Int16 deriving (Show)

type SArr = SFunArray

type S = SArr Int16 Int16

carried :: Int16
carried = 255

initState :: Game -> SArr Int16 Int16 -> S
initState Game{..} itemsArr = writeArray itemsArr 0 255

step :: Game -> (SInt16, SInt16) -> S -> (SBool, S)
step Game{..} (verb, noun) locs = (finished, locs')
  where
    finished = readArray locs' (literal 0) .== literal gameTreasury

    locs' = ite (verb .== 10) builtin_get $
            ite (verb .== 18) builtin_drop $
            locs

    builtin_get = ite (readArray locs item .== literal gameStartRoom) (writeArray locs item 255) locs
    builtin_drop = ite (readArray locs item .== literal carried) (writeArray locs item (literal gameStartRoom)) locs

    item = SBV.fromMaybe (-1) $ sFindIndex (\(Item name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems gameItems

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = SBV.sNothing
    go i (x:xs) = ite (p x) (SBV.sJust i) (go (i + 1) xs)
