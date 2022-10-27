{-# LANGUAGE UnicodeSyntax #-}

module Hake.Operators
  ( (##>)
  , (#>)
  , (@>)
  , (@@>)
  , (∫)
  , (∰)
  , (◉)
  , (♯)
  , (♯♯)
  ) where

import           Hake.Syntax

-- operators
infixl 5 ◉
-- even >>= will have 1 priority and $ have priority 0
-- to check priority: type ":i >>=" into ghci
infixl 0 ∰, ∫, #>, ##>, @>, @@>, ♯, ♯♯

-- tuple maker
(◉) ∷ String → [String] → (String, [String])
s ◉ ss = (s, ss)

-- Phony operator
(@>) ∷ String → IO () → IO ()
r @> a = phony r a

-- Phony' operator
(@@>) ∷ (String, [String]) → IO () → IO ()
(r, d) @@> a = phony d r a

-- Unicode variant of phony
(∫) ∷ String → IO () → IO ()
r ∫ a = phony r a

-- Unicode variant of phony'
(∰) ∷ (String, [String]) → IO () → IO ()
(r, d) ∰ a = phony d r a

-- Obj operator
(#>) ∷ String → IO () → IO ()
r #> a = obj r a

-- Obj' operator
(##>) ∷ (String, [String]) → IO () → IO ()
(r, d) ##> a = obj d r a

-- Unicode Obj operator
(♯) ∷ FilePath → IO () → IO ()
r ♯ a = obj r a

-- Unicode Obj' operator
(♯♯) ∷ (FilePath, [String]) → IO () → IO ()
(r, d) ♯♯ a = obj d r a
