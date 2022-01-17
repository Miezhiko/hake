{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hake.Lang.C
  ( cmake
  , cmakeBuild
  , nmake
  , vcshell
  , qmake
  ) where

import           Control.Monad
import           Hake.Core

cmake ∷ [String] → IO ()
cmake a = rawSystem "cmake" a >>= checkExitCode

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
qmake α = rawSystem "qmake" α >>= checkExitCode
