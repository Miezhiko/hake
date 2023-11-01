{-# LANGUAGE
    FlexibleContexts
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

phony ∷ (Optional1 [String] (String -> IO () -> IO ()) r) ⇒ r
phony = opt1 gPhony []

gPhony ∷ [String] -> String -> IO () -> IO ()
gPhony deps arg phonyAction = do
  myPhonyArgs <- readIORef phonyArgs
  let (an, de) = nameAndDesc arg
  if an ∈ myPhonyArgs
    then do
      -- we don't just compilePhony here because
      -- we leave early if phony is on our args
      let filtered = filter (/= an) myPhonyArgs
      writeIORef phonyArgs filtered
      unless (null deps) $ do
        for_ deps $ \dep -> do
          myPhonyActions <- readIORef phonyActions
          myObjects      <- readIORef objects
          for_ (M.lookup dep myObjects) (compileObj False dep)
          for_ (M.lookup dep myPhonyActions) $ \(depPhonyAction, _) ->
            compilePhony dep (depPhonyAction, S.empty)
      phonyAction
      when (null filtered) exitSuccess
    else do myPhonyActions <- readIORef phonyActions
            writeIORef phonyActions $
              M.insert an (phonyAction, de) myPhonyActions

obj ∷ (Optional1 [String] (FilePath -> IO () -> IO ()) r) ⇒ r
obj = opt1 gObj []

gObj ∷ [String] -> FilePath -> IO () -> IO ()
gObj deps arg buildAction = do
  currentObjects    <- readIORef objects
  currentObjectList <- readIORef objectsSet
  let new = M.insert arg (buildAction, S.fromList deps) currentObjects
  writeIORef objectsSet (S.insert arg currentObjectList)
  writeIORef objects new
