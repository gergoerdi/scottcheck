module ScottCheck.Utils where

import Data.Array as A

import Data.SBV
import Data.SBV.Maybe (sJust, sNothing)

(.!) :: (Mergeable a) => Array Int16 a -> SInt16 -> a
xs .! i = case A.elems xs of
    [] -> error "(.!) : empty array"
    xs@(x:_) -> select xs x i

replaceAt :: (Mergeable a) => SInt16 -> a -> Array Int16 a -> Array Int16 a
replaceAt i x' xs = A.listArray (A.bounds xs) . map (\(j, x) -> ite (i .== literal j) x' x) . A.assocs $ xs


sWhen :: (Monad m, Mergeable (m ())) => SBool -> m () -> m ()
sWhen b act = ite b act (return ())

sUnless :: (Monad m, Mergeable (m ())) => SBool -> m () -> m ()
sUnless b act = ite b (return ()) act

count :: (Mergeable n, Num n) => (a -> SBool) -> [a] -> n
count p xs = go xs
  where
    go [] = 0
    go (x:xs) = ite (p x) 1 0 + go xs

sCase :: (Mergeable a) => SInt16 -> a -> [(Int16, a)] -> a
sCase x def = go
  where
    go [] = def
    go ((k,v):kvs) = ite (x .== literal k) v (go kvs)

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = sNothing
    go i (x:xs) = ite (p x) (sJust i) (go (i + 1) xs)
