{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Main
  ( main
  ) where

import           HakeScript
import           Version

import           Data.Kind             (Type)

import           System.Console.GetOpt
import           System.IO

main ∷ IO ()
main = do
  hakeArgs <- getArgs
  current  <- getCurrentDirectory
  let (actions, _, _) = getOpt RequireOrder hakeOptions hakeArgs
  Options { optForce    = force
          , optPretend  = test
          } <- foldl (>>=) (pure defaultOptions) actions
  hakeIt hakeArgs current force test

data Options
  = Options
      { optForce   :: Bool
      , optPretend :: Bool
      }

defaultOptions ∷ Options
defaultOptions = Options {
    optForce    = False
  , optPretend  = False
  }

hakeOptions ∷ [OptDescr (Options -> IO Options)]
hakeOptions = [
  Option "v" ["version"]  (NoArg showV)         "Display Version",
  Option "h" ["help"]     (NoArg displayHelp)   "Display Help",
  Option "f" ["force"]    (NoArg forceRebuild)  "force script rebuild",
  Option "P" ["pretend"]  (NoArg pretend)       "pretend building (testing hake script)"
  ]

forceRebuild ∷ ∀ (m ∷ Type -> Type). Monad m ⇒ Options -> m Options
pretend      ∷ ∀ (m ∷ Type -> Type). Monad m ⇒ Options -> m Options

-- note ο is not o but greek ο!
forceRebuild ο  = pure ο { optForce = True }
pretend ο       = pure ο { optPretend = True }

displayHelp ∷ Options -> IO Options
displayHelp ο = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo prg hakeOptions)
  pure ο { optForce = True }
