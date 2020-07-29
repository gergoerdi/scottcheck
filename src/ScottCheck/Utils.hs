module ScottCheck.Utils where

import Data.Array as A
import Data.Int

replaceAt :: Int16 -> a -> Array Int16 a -> Array Int16 a
replaceAt i x' xs = A.listArray (A.bounds xs) . map (\(j, x) -> if (i == j) then x' else x) . A.assocs $ xs

count :: (Num n) => (a -> Bool) -> [a] -> n
count p xs = go xs
  where
    go [] = 0
    go (x:xs) = (if p x then 1 else 0) + go xs
