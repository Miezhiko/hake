module Hake.Lang.Haskell
  ( cabal
  , cabalBuild
  , cabalConfigure
  , cleanCabalLocal
  , elm
  , getCabalBuildPath
  , ghc
  , stack
  ) where

import           Hake.Common
import           Hake.StripUtils

import           Control.Monad          (unless)

import           Data.Foldable          (traverse_)
import           Data.List              (isPrefixOf)

import           Hake.Helper.FileSystem (removeIfExists)

ghc ∷ [String] -> IO ()
ghc = raw "ghc"

elm ∷ [String] -> IO ()
elm = raw "elm"

cabal ∷ [String] -> IO ()
cabal = raw "cabal"

stack ∷ [String] -> IO ()
stack = raw "stack"

cabalConfigure ∷ IO ()
cabalConfigure = do
  cwd' <- getCurrentDirectory
  let localProjectFile = cwd' </> "cabal.project.local"
  ex <- doesFileExist localProjectFile
  unless ex $ cabal ["configure"]

cabalBuild ∷ IO ()
cabalBuild = cabal ["build"]

cleanCabalLocal ∷ IO ()
cleanCabalLocal =
  getCurrentDirectory >>= getDirectoryContents
                      >>= traverse_ removeIfExists
       ∘ filter (isPrefixOf "cabal.project.local~")

getCabalBuildPath ∷ String -> IO String
getCabalBuildPath appName = do
  (exitCode, stdOut, _stdErr) <-
      readProcessWithExitCode "cabal" ["list-bin", appName] []
  if exitCode == ExitSuccess
      then pure $ rstrip stdOut
      else pure []
