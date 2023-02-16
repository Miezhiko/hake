{-# LANGUAGE
    UnicodeSyntax
  #-}

module Hake.Lang.Rust
  ( cargo
  , rustc
  ) where

import           Hake.Common

cargo ∷ [String] -> IO ()
cargo = raw "cargo"

rustc ∷ [String] -> IO ()
rustc = raw "rustc"
