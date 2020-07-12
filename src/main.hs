module Main where

import Data.Int
import Data.SBV
import Data.SBV.Control
import qualified Data.SBV.Maybe as SBV

type SArr = SFunArray

type S = SArr Int16 Int16

initState :: SArr Int16 Int16 -> S
initState itemsArr = writeArray itemsArr 0 255

step :: [Maybe Int16] -> (SInt16, SInt16) -> S -> SBool
step items (verb, noun) locs = finished
  where
    finished = readArray locs' (literal 0) .== 1

    locs' = ite (verb .== 10) builtin_get $
            ite (verb .== 18) builtin_drop $
            locs

    builtin_get = ite (readArray locs item .== 1) (writeArray locs item 255) locs
    builtin_drop = ite (readArray locs item .== 255) (writeArray locs item 1) locs

    item = SBV.fromMaybe (-1) $ sFindIndex (\name -> maybe sFalse ((noun .==) . literal) name) $ items

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = SBV.sNothing
    go i (x:xs) = ite (p x) (SBV.sJust i) (go (i + 1) xs)
main :: IO ()
main = do
    let theGame = [Just 7]
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ do
            verb1 <- genWord
            noun1 <- genWord
            constrain $ step theGame (verb1, noun1) (initState arr)
            ensureSat
            mapM getValue [verb1, noun1]
    mapM_ print cmds
  where
    genWord = do
        word <- freshVar_
        constrain $ 0 .<= word .&& word .< literal 19
        return word
