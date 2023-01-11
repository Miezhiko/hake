{-# LANGUAGE
    UnicodeSyntax
  #-}

module Hake.Core
  ( module Hake.Global
  , module Prelude.Unicode
  , module SystemImports
  , checkExitCode
  , compileObj
  , compilePhony
  , exitWithError
  , nameAndDesc
  , removePhonyArg
  ) where

import           Prelude.Unicode

import           System.Directory   as SystemImports
import           System.Environment as SystemImports
import           System.Exit        as SystemImports
import           System.FilePath    as SystemImports
import           System.Info        as SystemImports
import           System.Process     as SystemImports

import           Data.IORef
import           Data.List.Split
import qualified Data.Set           as S
import           Data.String.Utils  (strip)

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

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = pure ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

removePhonyArg ∷ [String] → String → IO [String]
removePhonyArg args arg = do
  let filtered = filter (/= arg) args
  writeIORef phonyArgs filtered
  pure filtered

compilePhony ∷ String → IO () → IO ()
compilePhony rule phonyAction = do
  phonyAction
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $
    void $ removePhonyArg myPhonyArgs rule

compileObj ∷ String → IO () → IO ()
compileObj file buildAction = do
  currentObjectList ← readIORef objectsSet
  when (S.member file currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fullPath = currentDir </> file
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists $ exitWithError (fullPath ++ " doesn't exists")
