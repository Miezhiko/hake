{-# LANGUAGE
    LambdaCase
  , MultiWayIf
  , UnicodeSyntax
  #-}

module Hake
  ( module HakeLib
  , hake
  ) where

import           Data.Foldable          (for_)
import           Data.IORef

import           Hake.Core              as HakeLib

import           Hake.Helper.FileSystem as HakeLib
import           Hake.Helper.Syntax     as HakeLib
import           Hake.Helper.Systemd    as HakeLib
import           Hake.Helper.Utils      as HakeLib

import           Hake.Lang.C            as HakeLib
import           Hake.Lang.Haskell      as HakeLib
import           Hake.Lang.Idris2       as HakeLib
import           Hake.Lang.Rust         as HakeLib

import           Hake.Operators         as HakeLib
import           Hake.Syntax            as HakeLib

hake ∷ IO () → IO ()
hake maybeAction = do
  args ← getArgs
  writeIORef phonyArgs args
  maybeAction
  {- HLINT ignore "Redundant multi-way if" -}
  if | "-h" ∈ args ∨ "--help" ∈ args → displayHelp
     | otherwise → do
        myObjects ← readIORef objects
        for_ myObjects $ uncurry compileObj

displayHelp ∷ IO ()
displayHelp =
  readIORef phonyActions
  >>= \case [] -> return ()
            xs -> do
              putStrLn "Current HakeScript options:"
              for_ (reverse xs) $ \(r, _, d) →
                putStrLn $ "  " ++ r ++ " :" ++ d
