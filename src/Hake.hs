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
import qualified Data.Set               as S

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

type Object = (String, (IO (), S.Set String))

maxDepsObject ∷ [Object] → Object
maxDepsObject = foldr1 (\
  (a1, (b1, d1)) (a2, (b2, d2)) ->
    if S.size d1 > S.size d2
      then (a1, (b1, d1))
      else (a2, (b2, d2)))

buildObjects ∷ [String] → [Object] → IO ()
buildObjects _ [(f, bd)] = compileObj True f bd
buildObjects [] objs =
  let (defaultObjectName, _) = maxDepsObject objs
  in for_ objs $ \(f, bd) →
    if f == defaultObjectName
      then compileObj True f bd
      else compileObj False f bd
buildObjects args objs =
  let objectsInArgs = filter ((∈ args) . fst) objs
  in case objectsInArgs of
    [] -> buildObjects [] objs
    xs -> for_ objs $ \(f, bd) →
            let filtereredArgs = map fst xs
            in if f ∈ filtereredArgs
              then compileObj True f bd
              else compileObj False f bd

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
