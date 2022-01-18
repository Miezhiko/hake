{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE UnicodeSyntax  #-}

import           HakeScript
import           Version

import           Foreign.Storable      (sizeOf)

import           System.Console.GetOpt
import           System.IO

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad

main ∷ IO ()
main = do
  hakeArgs ← getArgs
  current   ← getCurrentDirectory
  let (actions, _, _) = getOpt RequireOrder hakeOptions hakeArgs
  Options { optForce    = force
          , optPretend  = test
          } ← foldl (>>=) (return defaultOptions) actions
  hakeIt hakeArgs current force test

data Options = Options
  { optForce    ∷ Bool
  , optPretend  ∷ Bool
  }

defaultOptions ∷ Options
defaultOptions = Options {
    optForce    = False
  , optPretend  = False
  }

hakeOptions ∷ [OptDescr (Options → IO Options)]
hakeOptions = [
  Option "v" ["version"]  (NoArg showV)             "Display Version",
  Option "h" ["help"]     (NoArg displayHelp)       "Display Help",
  Option "f" ["force"]    (NoArg forceRebuild)      "force script rebuild",
  Option "P" ["pretend"]  (NoArg pretend)           "pretend building (testing hake script)"
  ]

forceRebuild ∷ ∀ (m ∷ * → *). Monad m ⇒ Options → m Options
pretend      ∷ ∀ (m ∷ * → *). Monad m ⇒ Options → m Options

-- note ο is not o but greek ο!
forceRebuild ο  = return ο { optForce = True }
pretend ο       = return ο { optPretend = True }

displayHelp :: Options → IO Options
displayHelp ο = do
  prg ← getProgName
  hPutStrLn stderr (usageInfo prg hakeOptions)
  return ο { optForce = True }
