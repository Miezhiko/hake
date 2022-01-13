{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Version
  ( showHelp
  , showV
  , printVersion
  ) where

import           Text.Printf

import           System.Console.GetOpt
import           System.Exit

import           Data.Version          (showVersion)
import qualified Paths_hake            as My

printVersion ∷ IO ()
printVersion = putStrLn $ showVersion My.version

showMyV     ∷ String
showMyV      = showVersion My.version

showV       ∷ ∀ τ β. τ → IO β
showV _      = putStrLn ("Hake v." ++ showMyV) >> exitSuccess

showHelp    ∷ ∀ τ β α. [OptDescr α] → τ → IO β
showHelp o _ = putStrLn (usageInfo "Usage: hake" o)
                  >> exitSuccess
