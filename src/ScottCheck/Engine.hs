module ScottCheck.Engine (Item(..), initState, step) where

import Data.Int
import Data.Array as A

import Data.SBV
import qualified Data.SBV.Maybe as SBV

data Item = Item (Maybe Int16) Int16 deriving (Show)

type SArr = SFunArray

type S = SArr Int16 Int16

carried :: Int16
carried = 255

initState :: SArr Int16 Int16 -> S
initState itemsArr = writeArray itemsArr 0 255

step :: Array Int16 Item -> (SInt16, SInt16) -> S -> (SBool, S)
step items (verb, noun) locs = (finished, locs')
  where
    finished = readArray locs' (literal 0) .== 1

    locs' = ite (verb .== 10) builtin_get $
            ite (verb .== 18) builtin_drop $
            locs

    builtin_get = ite (readArray locs item .== 1) (writeArray locs item 255) locs
    builtin_drop = ite (readArray locs item .== literal carried) (writeArray locs item 1) locs

    item = SBV.fromMaybe (-1) $ sFindIndex (\(Item name _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = SBV.sNothing
    go i (x:xs) = ite (p x) (SBV.sJust i) (go (i + 1) xs)
