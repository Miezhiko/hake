module Hake.Global
  ( objects
  , objectsSet
  , phonyActions
  , phonyArgs
  ) where

import           System.IO.Unsafe

import           Data.IORef
import qualified Data.Map         as M
import qualified Data.Set         as S

-- command line arguments
phonyArgs ∷ IORef [String]
{-# NOINLINE phonyArgs #-}
phonyArgs = unsafePerformIO     $ newIORef []

-- parsed phony actions to use with arguments
phonyActions ∷ IORef (M.Map String (IO (), String))
{-# NOINLINE phonyActions #-}
phonyActions = unsafePerformIO $ newIORef M.empty

-- parsed objects (whit set of their deps)
objects ∷ IORef (M.Map String (IO (), S.Set String))
{-# NOINLINE objects #-}
objects = unsafePerformIO      $ newIORef M.empty

-- helper set of objects for small runtime optimization
objectsSet ∷ IORef (S.Set String)
{-# NOINLINE objectsSet #-}
objectsSet = unsafePerformIO   $ newIORef S.empty
