{-# LANGUAGE
    CPP
  , UnicodeSyntax
  #-}

module Hake.Lang.C
  ( cmake
  , cmakeBuild
  , nmake
  , qmake
  , vcshell
  ) where

import           Hake.Common

cmake ∷ [String] → IO ()
cmake = raw "cmake"

cmakeBuild ∷ [String] → IO ()
cmakeBuild α = rawSystem "cmake" ("--build" : α) >>= checkExitCode

vcshell ∷ [String] → IO String
nmake ∷ [String] → IO ()

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
vcshell [x] = do
  common ← getEnv $ "VS" ++ x ++ "COMNTOOLS"
  return $ common </> ".."  </> ".."
                  </> "VC"
                  </> "vcvarsall.bat"
vcshell (x:xs) = do
  vcx ← vcshell [x]
  if vcx /= [] then return vcx
               else vcshell xs
vcshell []      = return []

nmake α = rawSystem "nmake" α >>= checkExitCode
#else
vcshell _ = return []
nmake   _ = return ()
#endif

qmake ∷ [String] → IO ()
qmake = raw "qmake"
