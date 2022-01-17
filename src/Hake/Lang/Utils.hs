{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Utils
    ( make
    , configure
    , curl
    , git
    , grep
    , sed
    ) where

import           Control.Monad
import           Hake.Core

make ∷ [String] → IO ()
make α = rawSystem "curl" α >>= checkExitCode

configure ∷ [String] → IO ()
configure α = rawSystem "configure" α >>= checkExitCode

curl ∷ [String] → IO ()
curl α = rawSystem "curl" α >>= checkExitCode

git ∷ [String] → IO ()
git α = rawSystem "git" α >>= checkExitCode

grep ∷ [String] → IO ()
grep α = rawSystem "git" α >>= checkExitCode

sed ∷ [String] → IO ()
sed α = rawSystem "git" α >>= checkExitCode
