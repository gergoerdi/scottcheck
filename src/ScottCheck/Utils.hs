{-# LANGUAGE RebindableSyntax #-}
module ScottCheck.Utils where

import Prelude
import Data.Array as A

import Data.SBV
import Data.SBV.Control
import Control.Monad.State
import Data.SBV.Maybe (sJust, sNothing)

ifThenElse :: (Mergeable a) => SBool -> a -> a -> a
ifThenElse = ite

(.!) :: (Mergeable a) => Array Int16 a -> SInt16 -> a
xs .! i = case A.elems xs of
    [] -> error "(.!) : empty array"
    xs@(x:_) -> select xs x i

replaceAt :: (Mergeable a) => SInt16 -> a -> Array Int16 a -> Array Int16 a
replaceAt i x' xs = A.listArray (A.bounds xs) . map update . A.assocs $ xs
  where
    update (j, x) = if i .== literal j then x' else x

sWhen :: (Monad m, Mergeable (m ())) => SBool -> m () -> m ()
sWhen b act = if b then act else return ()

sUnless :: (Monad m, Mergeable (m ())) => SBool -> m () -> m ()
sUnless b act = if b then return () else act

count :: (Mergeable n, Num n) => (a -> SBool) -> [a] -> n
count p xs = go xs
  where
    go [] = 0
    go (x:xs) = if p x then 1 else 0 + go xs

sCase :: (Mergeable a) => SInt16 -> [(Int16, a)] -> a -> a
sCase x cases def = go cases
  where
    go [] = def
    go ((k,v):kvs) = if x .== literal k then v else go kvs

sFindIndex :: (a -> SBool) -> [a] -> SMaybe Int16
sFindIndex p = go 0
  where
    go i [] = sNothing
    go i (x:xs) = if p x then sJust i else go (i + 1) xs


loopState :: (SymVal i) => (Int -> Query (SBV i)) -> s -> (SBV i -> State s SBool) -> Query [i]
loopState genCmd s0 step = go 1 s0 []
  where
    go i s cmds = do
        io $ putStrLn $ "Searching at depth: " ++ show i

        cmd <- genCmd i
        let cmds' = cmds ++ [cmd]

        push 1
        let (finished, s') = runState (step cmd) s
        constrain finished
        cs <- checkSat

        case cs of
            Unk -> error $ "Solver said Unknown, depth: " ++ show i
            Unsat -> do
                pop 1
                go (i+1) s' cmds'
            Sat -> mapM getValue cmds'
