{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Rust
    ( cargo
    , rustc
    ) where

import           Control.Monad
import           Hake.Core

cargo ∷ [String] → IO ()
cargo α = rawSystem "cargo" α >>= checkExitCode

rustc ∷ [String] → IO ()
rustc α = rawSystem "rustc" α >>= checkExitCode
