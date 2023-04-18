module Hake.Common
  ( module SystemImports
  , checkExitCode
  , raw
  ) where

import           Prelude.Unicode    as SystemImports

import           System.Directory   as SystemImports
import           System.Environment as SystemImports
import           System.Exit        as SystemImports
import           System.FilePath    as SystemImports
import           System.Info        as SystemImports
import           System.Process     as SystemImports

import           Control.Exception  as SystemImports

checkExitCode ∷ ExitCode -> IO ()
checkExitCode ExitSuccess = pure ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

raw ∷ String -> [String] -> IO ()
raw λ α = rawSystem λ α >>= checkExitCode
