{-# LANGUAGE UnicodeSyntax #-}

module Hake.Core
  ( checkExitCode
  , exitWithError
  , nameAndDesc
  , removePhonyArg
  , compilePhony
  , compileObj
  , module SystemImports
  , module Prelude.Unicode
  , module Hake.Global
  ) where

import           Prelude.Unicode

import           System.Directory   as SystemImports
import           System.Environment as SystemImports
import           System.Exit        as SystemImports
import           System.FilePath    as SystemImports
import           System.Info        as SystemImports
import           System.Process     as SystemImports

import           Data.Char          (isSpace)
import           Data.IORef
import           Data.List.Split

import           Control.Monad

import           Hake.Global

exitWithError ∷ String → IO ()
exitWithError μ = do putStrLn $ "Error: " ++ μ
                     exitFailure

trim ∷ String → String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail ∷ String → String → String
dropSpaceTail _ "" = ""
dropSpaceTail maybeStuff (χ:xs)
  | isSpace χ       = dropSpaceTail (χ:maybeStuff) xs
  | null maybeStuff = χ : dropSpaceTail "" xs
  | otherwise       = reverse maybeStuff ++ χ : dropSpaceTail "" xs

nameAndDesc ∷ String → (String, String)
nameAndDesc χ =
  let splt = splitOn "|" χ
  in if length splt > 1
      then (trim (head splt), last splt)
      else (trim χ, "No description")

checkExitCode ∷ ExitCode → IO ()
checkExitCode ExitSuccess = return ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

removePhonyArg ∷ [String] → String → IO [String]
removePhonyArg args arg = do
  let filtered = filter (/= arg) args
  writeIORef phonyArgs filtered
  return filtered

compilePhony ∷ String → IO () → IO ()
compilePhony rule phonyAction = do
  phonyAction
  myPhonyArgs ← readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $ do
    removePhonyArg myPhonyArgs rule
    return ()

compileObj ∷ String → IO () → IO ()
compileObj file buildAction = do
  currentObjectList ← readIORef objectsList
  when (file ∈ currentObjectList) $ do
    currentDir ← getCurrentDirectory
    let fullPath = currentDir </> file
    buildAction -- building this file
    objExists ← doesFileExist fullPath
    unless objExists $ exitWithError (fullPath ++ " doesn't exists")
