{-# LANGUAGE
    RankNTypes
  #-}

module Script
  ( getMTime
  , runHake
  ) where

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import           System.Posix.Files
import           System.Posix.Process
import           System.Posix.Types
#endif

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
