{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Js
    ( npm
    , yarn
    , vue
    ) where

import           Hake.Common

npm ∷ [String] → IO ()
npm = raw "npm"

yarn ∷ [String] → IO ()
yarn = raw "yarn"

vue ∷ [String] → IO ()
vue = raw "vue"
