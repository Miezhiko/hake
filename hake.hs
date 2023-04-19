{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  #-}

module Main where

import           Hake

main ∷ IO ()
main = hake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean | clean the project" ∫
    cabal ["clean"] `finally` removeDirIfExists buildPath
                           >> cleanCabalLocal

  "stack | build using stack" ∫
    stack ["--local-bin-path", buildPath, "--copy-bins", "build"]

  -- building object rule #> is non-unicode operator alternative
  hakeExecutable ♯ hakeBuild `finally` cleanCabalLocal

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install | install to system" ◉ [hakeExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

  -- build and run with --version
  "test | build and test" ◉ [hakeExecutable] ∰
    rawSystem hakeExecutable ["--version"]
      >>= checkExitCode

  -- build and run custom command from args
  "run | run compiled hake" ◉ [hakeExecutable] ∰
    getHakeArgs >>= rawSystem hakeExecutable
                >>= checkExitCode

 where
  appName ∷ String
  appName = "hake"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  hakeExecutable ∷ String
  hakeExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ ".exe"
       | otherwise                             -> buildPath </> appName

  copyExecutable ∷ String -> IO ()
  copyExecutable = flip copyFile hakeExecutable

  hakeBuild ∷ IO ()
  hakeBuild = do
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]
    cabal ["configure"]
    cabal ["build"]
    getCabalBuildPath ("exe:" ++ appName) >>= copyExecutable
