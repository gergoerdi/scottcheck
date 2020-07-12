module Main where

import Data.Int
import Data.SBV
import Data.SBV.Control

type SArr = SFunArray

type S = SArr Int16 Int16

step :: [Maybe Int16] -> (SInt16, SInt16) -> S -> SBool
step items (verb, noun) locs = readArray locs' 0 .== 1
  where
    locs' = ite (verb .== 0) builtin_get $
            ite (verb .== 1) builtin_drop $
            locs

    builtin_get = writeArray locs item $ let loc = readArray locs item in ite (loc .== 1) 255 loc
    builtin_drop = writeArray locs item $ let loc = readArray locs item in ite (loc .== 255) 1 loc

    item = ite (noun .== 7) 0 (-1)

main :: IO ()
main = do
    let theGame = [Just 7]
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ do
            verb <- freshVar "verb"
            noun <- freshVar "noun"
            constrain $ step theGame (verb, noun) (writeArray arr 0 255)
            ensureSat
            mapM getValue [verb, noun]
    mapM_ print cmds
