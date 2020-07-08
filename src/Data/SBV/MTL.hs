{-# LANGUAGE QuantifiedConstraints #-}
module Data.SBV.MTL where

import Data.SBV

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict

instance (Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (ReaderT r m a) where
    symbolicMerge force cond thn els = ReaderT $ symbolicMerge force cond (runReaderT thn) (runReaderT els)

instance (Mergeable s, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Strict.StateT s m a) where
    symbolicMerge force cond thn els = Strict.StateT $
        symbolicMerge force cond (Strict.runStateT thn) (Strict.runStateT els)

instance (Mergeable s, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Lazy.StateT s m a) where
    symbolicMerge force cond thn els = Lazy.StateT $
        symbolicMerge force cond (Lazy.runStateT thn) (Lazy.runStateT els)

instance forall a. Mergeable a => Mergeable (Identity a) where
    symbolicMerge force cond thn els = Identity $ symbolicMerge force cond (runIdentity thn) (runIdentity els)

instance (Mergeable w, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Strict.WriterT w m a) where
    symbolicMerge force cond thn els = Strict.WriterT $
        symbolicMerge force cond (Strict.runWriterT thn) (Strict.runWriterT els)

instance (Mergeable w, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Lazy.WriterT w m a) where
    symbolicMerge force cond thn els = Lazy.WriterT $
        symbolicMerge force cond (Lazy.runWriterT thn) (Lazy.runWriterT els)

instance (Mergeable s, Mergeable w, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Strict.RWST r w s m a) where
        symbolicMerge force cond thn els = Strict.RWST $
            symbolicMerge force cond (Strict.runRWST thn) (Strict.runRWST els)

instance (Mergeable s, Mergeable w, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (Lazy.RWST r w s m a) where
        symbolicMerge force cond thn els = Lazy.RWST $
            symbolicMerge force cond (Lazy.runRWST thn) (Lazy.runRWST els)
