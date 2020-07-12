{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine (Game(..), Item(..), initState, stepPlayer) where

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

type SInput = (SInt16, SInt16)

type SArr = SFunArray

type S = (SInt16, SArr Int16 Int16)

carried :: Int16
carried = 255

initState :: Game -> SArr Int16 Int16 -> S
initState Game{..} itemsArr = (literal gameStartRoom, writeArray itemsArr 0 255)

stepPlayer :: Game -> SInput -> S -> (SBool, S)
stepPlayer game@Game{..} (verb, noun) s@(here, locs) = -- (finished, s')
    let s' = builtin game (verb, noun) s
    in (finished game s', s')

finished :: Game -> S -> SBool
finished Game{..} (_, itemLocs) = readArray itemLocs (literal 0) .== literal gameTreasury

builtin :: Game -> SInput -> S -> S
builtin Game{..} (verb, noun) s@(here, locs) = (here, locs')
  where
    locs' = ite (verb .== 10) builtin_get $
            ite (verb .== 18) builtin_drop $
            locs

    builtin_get = ite (readArray locs item .== here) (writeArray locs item 255) locs
    builtin_drop = ite (readArray locs item .== literal carried) (writeArray locs item here) locs

    item = SBV.fromMaybe (-1) $ sFindIndex (\(Item name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems gameItems

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = SBV.sNothing
    go i (x:xs) = ite (p x) (SBV.sJust i) (go (i + 1) xs)
