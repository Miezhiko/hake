module Hake.Lang.Idris2
  ( idris2
  ) where

import           Hake.Common

idris2 ∷ [String] -> IO ()
idris2 = raw "idris2"
