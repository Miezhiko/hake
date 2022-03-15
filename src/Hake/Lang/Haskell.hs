{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.Haskell
    ( ghc
    , cabal
    , stack
    , cleanCabalLocal
    ) where

import Hake.Common

import Hake.Helper.FileSystem (removeIfExists)
import Data.List              (isPrefixOf)
import Data.Foldable          (traverse_)
import Data.String.Utils      (rstrip)

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

getCabalAppName ∷ String → IO String
getCabalAppName appName = do
  (exitCode, stdOut, stdErr) ←
      readProcessWithExitCode "cabal" ["list-bin", appName] []
  if exitCode == ExitSuccess 
      then return $ rstrip stdOut
      else return []
