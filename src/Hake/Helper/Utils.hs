module Hake.Helper.Utils
  ( configure
  , curl
  , git
  , grep
  , make
  , sed
  ) where

import           Hake.Common

make ∷ [String] -> IO ()
make = raw "make"

configure ∷ [String] -> IO ()
configure = raw "configure"

curl ∷ [String] -> IO ()
curl = raw "curl"

git ∷ [String] -> IO ()
git = raw "git"

grep ∷ [String] -> IO ()
grep= raw "grep"

sed ∷ [String] -> IO ()
sed = raw "sed"
