{-# LANGUAGE UnicodeSyntax #-}

module Hake.Global
  ( objects
  , objectsList
  , phonyActions
  , phonyArgs
  ) where

import           System.IO.Unsafe

import           Data.IORef

phonyArgs ∷ IORef [String]
{-# NOINLINE phonyArgs #-}
phonyArgs = unsafePerformIO (newIORef [])

phonyActions ∷ IORef [(String, IO (), String)]
{-# NOINLINE phonyActions #-}
phonyActions = unsafePerformIO (newIORef [])

objects ∷ IORef [(String, IO ())]
{-# NOINLINE objects #-}
objects = unsafePerformIO (newIORef [])

objectsList ∷ IORef [String]
{-# NOINLINE objectsList #-}
objectsList = unsafePerformIO (newIORef [])
