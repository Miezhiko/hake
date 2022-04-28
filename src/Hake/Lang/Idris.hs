{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Idris
    ( idris
    ) where

import Hake.Common

idris ∷ [String] → IO ()
idris = raw "ghc"
