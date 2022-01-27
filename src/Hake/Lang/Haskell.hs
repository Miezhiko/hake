{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Haskell
    ( ghc
    , cabal
    , stack
    ) where

import Hake.Common

ghc ∷ [String] → IO ()
ghc = raw "ghc"

cabal ∷ [String] → IO ()
cabal = raw "cabal"

stack ∷ [String] → IO ()
stack = raw "stack"
