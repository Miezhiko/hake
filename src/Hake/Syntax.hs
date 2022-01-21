{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , UnicodeSyntax
  #-}

module Hake.Syntax
  ( obj
  , phony
  ) where

import           Data.Foldable (for_)
import           Data.IORef
import qualified Data.Set      as S

import           Control.Monad

import           Hake.Core
import           Hake.Optional

phony ∷ (Optional1 [String] (String → IO () → IO ()) r) ⇒ r
phony = opt1 gPhony []

gPhony ∷ [String] → String → IO () → IO ()
gPhony [] arg phonyAction = do
  args ← readIORef phonyArgs
  let (an, de) = nameAndDesc arg
  if an ∈ args
    then do phonyAction
            filtered ← removePhonyArg args an
            when (null filtered) exitSuccess
    else do currentPhony ← readIORef phonyActions
            let new = (an, phonyAction, de) : currentPhony
            writeIORef phonyActions new
gPhony deps arg complexPhonyAction = do
  myPhonyArgs     ← readIORef phonyArgs
  myPhonyActions  ← readIORef phonyActions
  let (an, de) = nameAndDesc arg
  if an ∈ myPhonyArgs
    then do
      myObjects ← readIORef objects
      for_ deps $ \dep → do
        for_ myObjects $ \(file, buildAction) →
          when (dep == file) $
            compileObj file buildAction
        for_ myPhonyActions $ \(rule, phonyAction, _) →
          when (dep == rule) $ compilePhony rule phonyAction
      complexPhonyAction
      filtered ← removePhonyArg myPhonyArgs an
      when (null filtered) exitSuccess
    else let new = (an, complexPhonyAction, de) : myPhonyActions
         in writeIORef phonyActions new

obj ∷ (Optional1 [String] (FilePath → IO () → IO ()) r) ⇒ r
obj = opt1 gObj []

gObj ∷ [String] → FilePath → IO () → IO ()
gObj [] arg buildAction = do
  currentObjects ← readIORef objects
  currentObjectList ← readIORef objectsSet
  let new = (arg, buildAction) : currentObjects
  writeIORef objectsSet (S.insert arg currentObjectList)
  writeIORef objects new
gObj deps arg complexBuildAction = do
  myPhonyActions ← readIORef phonyActions
  myObjects      ← readIORef objects
  myObjectList   ← readIORef objectsSet
  for_ deps $ \dep → do
    for_ myObjects $ \(file, buildAction) →
      when (dep == file) $
        compileObj file buildAction
    for_ myPhonyActions $ \(rule, phonyAction, _) →
      when (dep == rule) $ compilePhony rule phonyAction
  let new = (arg, complexBuildAction) : myObjects
  writeIORef objectsSet (S.insert arg myObjectList)
  writeIORef objects new
