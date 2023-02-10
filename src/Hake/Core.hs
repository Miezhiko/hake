{-# LANGUAGE
    UnicodeSyntax
  #-}

module Hake.Core
  ( compileObj
  , compilePhony
  , nameAndDesc
  ) where

import           Prelude.Unicode

import           System.Directory
import           System.Exit
import           System.FilePath

import           Data.Foldable     (for_)
import           Data.IORef
import           Data.List.Split
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Data.String.Utils (strip)

import           Control.Monad

import           Hake.Global

exitWithError ∷ String → IO ()
exitWithError μ = do putStrLn $ "Error: " ++ μ
                     exitFailure

nameAndDesc ∷ String → (String, String)
nameAndDesc χ =
  let splt = splitOn "|" χ
  in if length splt > 1
      then (strip (head splt), last splt)
      else (strip χ, "No description")

compilePhony ∷ String → IO () → IO ()
compilePhony rule phonyAction = do
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $
    writeIORef phonyArgs
      $ filter (/= rule) myPhonyArgs
  phonyAction

-- unless force will just check if file exists
compileObj ∷ Bool → String → (IO (), S.Set String) → IO ()
compileObj force file (buildAction, deps) = do
  currentObjectList ← readIORef objectsSet
  when (S.member file currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fullPath = currentDir </> file
    objExists ← doesFileExist fullPath
    when (not objExists || force) $ do
      -- TODO: diagnose recursion to avoid stuck on runtime
      unless (S.null deps) $ do
        myPhonyActions  ← readIORef phonyActions
        myObjects       ← readIORef objects
        for_ (S.toList deps) $ \dep → do
          for_ (M.lookup dep myObjects) (compileObj False dep)
          -- TODO: here we don't check for phony deps
          for_ (M.lookup dep myPhonyActions) $ \(phonyAction, _) →
            compilePhony dep phonyAction
      buildAction -- building this file
      objExistsNow ← doesFileExist fullPath
      unless objExistsNow $ exitWithError (fullPath ++ " doesn't exists")
