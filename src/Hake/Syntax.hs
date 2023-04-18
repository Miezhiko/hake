{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Hake.Syntax
  ( obj
  , phony
  ) where

import           Prelude.Unicode

import           Data.Foldable   (for_)
import           Data.IORef
import qualified Data.Map        as M
import qualified Data.Set        as S

import           Control.Monad

import           System.Exit     (exitSuccess)

import           Hake.Core
import           Hake.Global
import           Hake.Optional

removePhonyArg ∷ [String] -> String -> IO [String]
removePhonyArg args arg =
  let filtered = filter (/= arg) args
  in writeIORef phonyArgs filtered
    >> pure filtered

phony ∷ (Optional1 [String] (String -> IO () -> IO ()) r) ⇒ r
phony = opt1 gPhony []

gPhony ∷ [String] -> String -> IO () -> IO ()
gPhony [] arg phonyAction = do
  args <- readIORef phonyArgs
  let (an, de) = nameAndDesc arg
  if an ∈ args
    then do filtered <- removePhonyArg args an
            phonyAction
            when (null filtered) exitSuccess
    else do currentPhony <- readIORef phonyActions
            let new = M.insert an (phonyAction, de) currentPhony
            writeIORef phonyActions new
gPhony deps arg complexPhonyAction = do
  myPhonyArgs     <- readIORef phonyArgs
  myPhonyActions  <- readIORef phonyActions
  let (an, de) = nameAndDesc arg
  if an ∈ myPhonyArgs
    then do
      myObjects <- readIORef objects
      for_ deps $ \dep -> do
        for_ (M.lookup dep myObjects) (compileObj False dep)
        for_ (M.lookup dep myPhonyActions) $ \(phonyAction, _) ->
          compilePhony dep phonyAction
      filtered <- removePhonyArg myPhonyArgs an
      complexPhonyAction
      when (null filtered) exitSuccess
    else let new = M.insert an (complexPhonyAction, de) myPhonyActions
         in writeIORef phonyActions new

obj ∷ (Optional1 [String] (FilePath -> IO () -> IO ()) r) ⇒ r
obj = opt1 gObj []

gObj ∷ [String] -> FilePath -> IO () -> IO ()
gObj deps arg buildAction = do
  currentObjects    <- readIORef objects
  currentObjectList <- readIORef objectsSet
  let new = M.insert arg (buildAction, S.fromList deps) currentObjects
  writeIORef objectsSet (S.insert arg currentObjectList)
  writeIORef objects new
