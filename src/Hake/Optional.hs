{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , Safe
  #-}

module Hake.Optional
  ( Optional1 (..)
  , Optional2 (..)
  ) where

class Optional1 α β ρ where
  opt1 :: (α -> β) -> α -> ρ

instance Optional1 α β β where
  opt1 = id

instance Optional1 α β (α -> β) where
  opt1 = const

class Optional2 α β γ ρ where
  opt2 :: (α -> β -> γ) -> α -> β -> ρ

instance Optional2 α β γ γ where
  opt2 = id

instance (Optional1 β γ ρ) ⇒ Optional2 α β γ (α -> ρ) where
  opt2 f _ b a = opt1 (f a) b
