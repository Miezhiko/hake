{-# LANGUAGE
    CPP
  , LambdaCase
  , MultiWayIf
  , UnicodeSyntax
  #-}

module HakeScript
  ( module Hake
  , hakeIt
  ) where

import           Hake
import           Script             (getMTime, runHake)

import           System.IO

import           Data.Maybe
import           Data.String.Utils

import           Control.Concurrent
import           Control.Exception
import           Control.Monad

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
readCheck     -- return whether command was success or not
  ∷ String    -- command
  -> [String] -- arguments
  -> IO (Either SomeException String)
readCheck γ args = try $ readProcess γ args []

checkIfSucc             -- check if success
  ∷ String              -- command to check
  -> [String]           -- arguments
  -> IO (Maybe String)  -- Just cmd in case of success
checkIfSucc γ args =
  readCheck γ args
    >>= \case Left _    -> pure Nothing
              Right val -> do putStr $ γ ⧺ " : " ⧺ val
                              pure $ Just γ

versionCheck          -- check for ghc --version
  ∷ String            -- command to check
  -> IO (Maybe String) -- Path to GHC in case of success
versionCheck γ = checkIfSucc γ ["--version"]

checkForStackGHC
  ∷ Maybe String
  -> IO (Maybe String)
checkForStackGHC γ = if isNothing γ
  then do
    localAppData <- getEnv("LOCALAPPDATA")
    let path = localAppData </> "Programs/stack/x86_64-windows/"
    stackPackages <- getDirectoryContents path
    let ghcPackages = filter (startswith "ghc") stackPackages
    stackGHV <-
      case ghcPackages of
        [] -> do appData <- getEnv("APPDATA")
                pure $ appData </> "local/bin/ghc.exe"
        xs -> let lastGHC = last xs
              in pure $ path </> lastGHC </> "bin/ghc.exe"
    versionCheck stackGHV
  else pure γ

getGHC ∷ IO String
getGHC = pure Nothing ≫= λ "ghc"
                      ≫= checkForStackGHC
                      ≫= (pure ◦ fromMaybe "ghc")
  where λ ∷ String -> Maybe String -> IO (Maybe String)
        λ χ prev = if isNothing prev
                      then versionCheck χ
                      else pure prev
#endif

hakeIt ∷ [String]
        -> String  -- current directory
        -> Bool    -- force
        -> Bool    -- pretend
        -> IO ()
hakeIt args current force pretend = do
  let fullNamelhs = current </> "hake.lhs"
      fullNamehs  = current </> "hake.hs"
      hakeHake    = hakeItF args current force pretend
  existslhs <- doesFileExist fullNamelhs
  existshs  <- doesFileExist fullNamehs
  if | existslhs -> hakeHake fullNamelhs
     | existshs  -> hakeHake fullNamehs
     | otherwise ->
        unless ("-h" ∈ args ∨ "--help" ∈ args) $
          putStrLn "no hake.hs / hake.lhs file"

hakeItF ∷ [String]
         -> String   -- current directory
         -> Bool     -- force
         -> Bool     -- pretend
         -> String   -- hake file
         -> IO ()
hakeItF args dir force pretend hakefile = do
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  -- on Windows GHC is not possibly in path (specially with stack)
  -- however we can look for it inside stack packages
  ghcCommand <- getGHC
#else
  let ghcCommand = "ghc"
#endif
  {- HLINT ignore "Redundant multi-way if" -}
  let cscr = if | os ∈ ["win32", "mingw32", "cygwin32"] -> "hake.exe"
                | otherwise                             -> "hake"

  cscrExists  <- doesFileExist cscr
  doRecompile <-
    if | force -> pure True
       | cscrExists -> do
          scrMTime  <- getMTime hakefile
          cscrMTime <- getMTime cscr
          pure $ cscrMTime <= scrMTime
       | otherwise -> pure True

  when doRecompile $ system (ghcCommand ++ " --make -o " ++ cscr ++ " " ++ hakefile)
                   >>= \case ExitFailure ε -> do
                               hPrint stderr ε
                               exitFailure
                             ExitSuccess -> pure ()

  let shArgs = if | force     -> filter (liftM2 (&&) ("-f" /=)
                                                     ("--force" /=)) args
                  | pretend   -> filter (liftM2 (&&) ("-p" /=)
                                                     ("--pretend" /=)) args
                  | otherwise -> args

  unless pretend $ runHake cscr shArgs
