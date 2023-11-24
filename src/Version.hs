{-# LANGUAGE
    RankNTypes
  #-}

module Version
  ( showHelp
  , showV
  ) where

import           System.Console.GetOpt
import           System.Exit

import           Data.Version          (showVersion)
import qualified Paths_hake            as My

showMyV      ∷ String
showMyV      = showVersion My.version

showV        ∷ ∀ τ β. τ -> IO β
showV _      = putStrLn ("Hake v" ++ showMyV) >> exitSuccess

showHelp     ∷ ∀ τ β α. [OptDescr α] -> τ -> IO β
showHelp o _ = putStrLn (usageInfo "Usage: hake" o)
                  >> exitSuccess
