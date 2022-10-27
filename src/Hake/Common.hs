{-# LANGUAGE UnicodeSyntax #-}

module Hake.Common
  ( module Hake.Core
  , raw
  ) where

import           Hake.Core

raw ∷ String → [String] → IO ()
raw λ α = rawSystem λ α >>= checkExitCode
