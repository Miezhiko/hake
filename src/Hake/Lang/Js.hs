{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Js
    ( npm
    , yarn
    , vue
    ) where

import           Control.Monad
import           Hake.Core

npm ∷ [String] → IO ()
npm α = rawSystem "npm" α >>= checkExitCode

yarn ∷ [String] → IO ()
yarn α = rawSystem "yarn" α >>= checkExitCode

vue ∷ [String] → IO ()
vue α = rawSystem "vue" α >>= checkExitCode
