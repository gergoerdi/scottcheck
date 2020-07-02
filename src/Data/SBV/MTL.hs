{-# LANGUAGE QuantifiedConstraints #-}
module Data.SBV.MTL where

import Data.SBV

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

instance (Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (ReaderT r m a) where
    symbolicMerge force cond thn els = ReaderT $ symbolicMerge force cond (runReaderT thn) (runReaderT els)

instance (Mergeable s, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (StateT s m a) where
    symbolicMerge force cond thn els = StateT $ symbolicMerge force cond (runStateT thn) (runStateT els)

instance forall a. Mergeable a => Mergeable (Identity a) where
    symbolicMerge force cond thn els = Identity $ symbolicMerge force cond (runIdentity thn) (runIdentity els)

instance (Mergeable w, Mergeable a, forall a. Mergeable a => Mergeable (m a)) => Mergeable (WriterT w m a) where
    symbolicMerge force cond thn els = WriterT $  symbolicMerge force cond (runWriterT thn) (runWriterT els)
