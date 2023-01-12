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
import           Data.List              (sortBy)
import qualified Data.Map               as M
import qualified Data.Set               as S

import           Control.Monad          (unless, when)

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

buildObjects ∷ [String] → [(String, (IO (), S.Set String))] → IO ()
buildObjects _ [(f, bd)] = compileObj True f bd
buildObjects [] objs =
  let sortedObjects =
        sortBy (\(_, (_, d1)) (_, (_, d2)) →
                  compare (S.size d1) (S.size d2)) objs
  in for_ sortedObjects $ uncurry (compileObj True)
buildObjects args objs =
  let objectsInArgs = filter ((∈ args) . fst) objs
  in case objectsInArgs of
    [] -> buildObjects [] objs
    xs -> for_ objs $ \(f, bd) →
            let filtereredArgs = map fst xs
            in when (f ∈ filtereredArgs) $
              compileObj True f bd

hake ∷ IO () → IO ()
hake maybeAction = do
  args ← getArgs
  writeIORef phonyArgs args
  maybeAction
  {- HLINT ignore "Redundant multi-way if" -}
  if | "-h" ∈ args ∨ "--help" ∈ args → displayHelp
     | otherwise → do
        myObjects ← readIORef objects
        unless (M.null myObjects) $
          buildObjects args (M.toList myObjects)

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
