{-# LANGUAGE UnicodeSyntax #-}

module Hake.Common
  ( raw
  , module Hake.Core
  ) where

import Hake.Core

raw ∷ String -> [String] → IO ()
raw λ α = rawSystem λ α >>= checkExitCode
