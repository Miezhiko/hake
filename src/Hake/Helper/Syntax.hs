{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hake.Helper.Syntax
  ( (<|)
  , (|>)
  , (<<|)
  , (|>>)
  ) where

infixl 2 <|, |>

(<|) ∷ (α → β) → α → β
f <| a = f a

(|>) ∷ α → (α → β) → β
a |> f = f a

infixl 7 <<|, |>>

(<<|) ∷ (α → β) → α → β
f <<| a = f a

(|>>) ∷ α → (α → β) → β
a |>> f = f a
