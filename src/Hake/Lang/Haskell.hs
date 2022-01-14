{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Haskell
    ( ghc
    , cabal
    , stack
    ) where

import           Control.Monad
import           Hake.Core

ghc ∷ [String] → IO ()
ghc α = rawSystem "ghc" α >>= checkExitCode

cabal ∷ [String] → IO ()
cabal α = rawSystem "cabal" α >>= checkExitCode

stack ∷ [String] → IO ()
stack α = rawSystem "stack" α >>= checkExitCode
