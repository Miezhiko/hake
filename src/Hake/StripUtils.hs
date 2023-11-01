{-# LANGUAGE
    Safe
  #-}

module Hake.StripUtils where

import           Prelude.Unicode

wschars ∷ String
wschars = " \t\r\n"

strip ∷ String -> String
strip = lstrip ∘ rstrip

lstrip ∷ String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

rstrip ∷ String -> String
rstrip = reverse ∘ lstrip ∘ reverse
