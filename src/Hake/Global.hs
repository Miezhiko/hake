{-# LANGUAGE
    UnicodeSyntax
  #-}

module Hake.Global
  ( objects
  , objectsSet
  , phonyActions
  , phonyArgs
  ) where

import           System.IO.Unsafe

import           Data.IORef
import qualified Data.Set         as S

-- command line arguments
phonyArgs ∷ IORef [String]
{-# NOINLINE phonyArgs #-}
phonyArgs = unsafePerformIO     $ newIORef []

-- parsed phony actions to use with arguments
phonyActions ∷ IORef [(String, IO (), String)]
{-# NOINLINE phonyActions #-}
phonyActions = unsafePerformIO  $ newIORef []

-- parsed objects
objects ∷ IORef [(String, IO ())]
{-# NOINLINE objects #-}
objects = unsafePerformIO       $ newIORef []

-- helper set of objects for small runtime optimization
objectsSet ∷ IORef (S.Set String)
{-# NOINLINE objectsSet #-}
objectsSet = unsafePerformIO   $ newIORef S.empty
