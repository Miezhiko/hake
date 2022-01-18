{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module HakeScript
  ( hakeIt
  , hakeItF
  , module Hake
  ) where

import           Script
import           Hake

import           System.IO

import           Data.String.Utils
import           Data.Maybe

import           Control.Concurrent
import           Control.Exception
import           Control.Monad

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
readCheck    -- return whether command was success or not
  ∷ String   -- command
  → [String] -- arguments
  → IO (Either SomeException String)
readCheck γ args = try $ readProcess γ args []

checkIfSucc           -- check if success
  ∷ String            -- command to check
  → [String]          -- arguments
  → IO (Maybe String) -- Just cmd in case of success
checkIfSucc γ args =
  readCheck γ args
    ≫= \case Left _ → return Nothing
             Right val → do putStr $ γ ⧺ " : " ⧺ val
                            return (Just γ)

versionCheck          -- check for ghc --version
  ∷ String            -- command to check
  → IO (Maybe String) -- Path to GHC in case of success
versionCheck γ = checkIfSucc γ ["--version"]

checkForStackGHC
  ∷ Maybe String
  → IO (Maybe String)
checkForStackGHC γ =
  if isNothing γ
    then do
      localAppData ← getEnv("LOCALAPPDATA")
      let path = localAppData </> "Programs/stack/x86_64-windows/"
      stackPackages ← getDirectoryContents path
      let ghcPackages = filter (startswith "ghc") stackPackages
      stackGHV ←
        case ghcPackages of
          [] → do appData ← getEnv("APPDATA")
                  return $ appData </> "local/bin/ghc.exe"
          xs → let lastGHC = last xs
               in return $ path </> lastGHC </> "bin/ghc.exe"
      versionCheck stackGHV
    else return γ

getGHC ∷ IO String
getGHC = return Nothing ≫= λ "ghc"
                        ≫= checkForStackGHC
                        ≫= \res → return $ fromMaybe "ghc" res
  where λ ∷ String → Maybe String → IO (Maybe String)
        λ χ prev = if isNothing prev
                      then versionCheck χ
                      else return prev
#endif

hakeIt ∷ [String]
        → String  -- current directory
        → Bool    -- force
        → Bool    -- pretend
        → IO ()
hakeIt args current force pretend = do
  let fullNamelhs = current </> "hake.lhs"
      fullNamehs  = current </> "hake.hs"
      hakeHake  = hakeItF args current force pretend
  existslhs ← doesFileExist fullNamelhs
  existshs  ← doesFileExist fullNamehs
  if | existslhs → hakeHake fullNamelhs
     | existshs  → hakeHake fullNamehs
     | otherwise →
        unless ("-h" ∈ args ∨ "--help" ∈ args) $
          putStrLn "no hake.hs / hake.lhs file"

hakeItF ∷ [String]
         → String   -- current directory
         → Bool     -- force
         → Bool     -- pretend
         → String   -- hake file
         → IO ()
hakeItF args dir force pretend hakefile = do
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  -- on Windows GHC is not possibly in path (specially with stack)
  -- however we can look for it inside stack packages
  ghcCommand ← getGHC
#else
  let ghcCommand = "ghc"
#endif
  {- HLINT ignore "Redundant multi-way if" -}
  let cscr = if | os ∈ ["win32", "mingw32", "cygwin32"] → "hake.exe"
                | otherwise → "hake"

  cscrExists  ← doesFileExist cscr
  doRecompile ←
    if | force → return True
       | cscrExists → do
          scrMTime  ← getMTime hakefile
          cscrMTime ← getMTime cscr
          return $ cscrMTime <= scrMTime
       | otherwise → return True

  when doRecompile $ system (ghcCommand ++ " --make -o " ++ cscr ++ " " ++ hakefile)
                   >>= \case ExitFailure ε → do
                               hPrint stderr ε
                               exitFailure
                             ExitSuccess → return ()

  -- TODO: filter out all the options
  let shArgs =
        if | force → filter (\ο → ο /= "-f"
                               && ο /= "--force") args
           | otherwise → args

  unless pretend $
    runHake cscr shArgs
