{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Haskell
  ( cabal
  , cleanCabalLocal
  , getCabalBuildPath
  , ghc
  , stack
  ) where

import           Hake.Common

import           Data.Foldable          (traverse_)
import           Data.List              (isPrefixOf)
import           Data.String.Utils      (rstrip)
import           Hake.Helper.FileSystem (removeIfExists)

ghc ∷ [String] → IO ()
ghc = raw "ghc"

cabal ∷ [String] → IO ()
cabal = raw "cabal"

stack ∷ [String] → IO ()
stack = raw "stack"

-- cabal build creates many cabal.project.local
-- I don't know why but those files look useless for me =_=
cleanCabalLocal ∷ IO ()
cleanCabalLocal =
  getCurrentDirectory >>= getDirectoryContents
                      >>= traverse_ removeIfExists
       . filter (isPrefixOf "cabal.project.local")

getCabalBuildPath ∷ String → IO String
getCabalBuildPath appName = do
  (exitCode, stdOut, _stdErr) ←
      readProcessWithExitCode "cabal" ["list-bin", appName] []
  if exitCode == ExitSuccess
      then return $ rstrip stdOut
      else return []
