{-# LANGUAGE
    CPP
  , RankNTypes
  , UnicodeSyntax
  #-}

module Script
  ( getMTime
  , runHake
  ) where

import           System.Directory     (getModificationTime)
import           System.Exit
import           System.IO
import           System.Process

import           Data.List
import           Data.Time.Clock

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import           System.Posix.Files
import           System.Posix.Process
import           System.Posix.Types
#endif

import           Control.Monad

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
getMTime ∷ FilePath -> IO UTCTime
getMTime = getModificationTime
#else
getMTime ∷ FilePath -> IO EpochTime
getMTime f = modificationTime <$> getFileStatus f
#endif

runHake ∷ String -> [String] -> IO ()
runHake cscr args =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  do pid <- runCommand (cscr ++ " " ++ intercalate " " args)
     waitForProcess pid >>= exitWith
#else
  executeFile cscr False args Nothing
#endif
