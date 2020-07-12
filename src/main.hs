{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Int
import Data.SBV
import Data.SBV.Control

type SArr = SFunArray

main :: IO ()
main = runSMT $ do
    arr <- newArray_ @SArr @Int16 @Int16 Nothing
    query $ do
        x <- freshVar_ @Int16
        let ys = writeArray arr 0 255
        constrain $
          let ys' = ite (x .== 0) ys1 $ ite (x .== 1) ys2 ys

              ys1 = writeArray ys idx $ ite (y .== 1) 255 y
              ys2 = writeArray ys idx $ ite (y .== 255) 1 y

              idx = ite (x .== 1) 0 (-1)
              y = readArray ys idx
          in readArray ys' 0 .== 1
        ensureSat
