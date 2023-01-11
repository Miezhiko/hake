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
import qualified Data.Map               as M

import           Control.Monad          (unless)

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
        for_ (M.toList myObjects) $ uncurry compileObj

displayHelp ∷ IO ()
displayHelp = do
  phonies <- readIORef phonyActions
  unless (M.null phonies) $ do
    putStrLn "Current HakeScript options:"
    let phoniesList = M.toList phonies
        maxNameLen = maximum $ map (length . fst) phoniesList
    for_ (reverse $ M.toList phonies) $ \(r, (_, d)) →
      let additionalSpacesCount = maxNameLen - length r
          spaces = replicate additionalSpacesCount ' '
      in putStrLn $ "  " ++ r ++ spaces ++ " :" ++ d
