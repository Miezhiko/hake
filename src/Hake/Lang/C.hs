module Hake.Lang.C where

import           Hake.Common

cmake ∷ [String] -> IO ()
cmake = raw "cmake"

cmakeBuild ∷ [String] -> IO ()
cmakeBuild α = rawSystem "cmake" ("--build" : α) >>= checkExitCode

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
vcshell ∷ [String] -> IO String
vcshell [x] = do
  common <- getEnv $ "VS" ++ x ++ "COMNTOOLS"
  pure $ common </> ".."  </> ".."
                </> "VC"
                </> "vcvarsall.bat"
vcshell (x:xs) = do
  vcx <- vcshell [x]
  if vcx /= [] then pure vcx
               else vcshell xs
vcshell []      = pure []

nmake ∷ [String] -> IO ()
nmake α = rawSystem "nmake" α >>= checkExitCode
#endif

qmake ∷ [String] -> IO ()
qmake = raw "qmake"
