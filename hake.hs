{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import Hake

main ∷ IO ()
main = hake $ do
  -- phony clean @> is non-unicode operator alternative
  "clean | clean the project" ∫ do
    stack ["clean"]
    removeDirectoryRecursive buildPath

  -- building object rule #> is non-unicode operator alternative
  hakeExecutable ♯ do
    stack ["--local-bin-path", buildPath, "--copy-bins", "build"]

  "cabal | build using cabal" ∫ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  -- install phony depending on obj, @@> is non-unicode operator alternative
  -- ##> or ♯♯ is for dependent object rule, ◉ is just uncarry operator
  "install | install to system" ◉ [hakeExecutable] ∰
    -- stack ["install"]
    cabal ["install", "--overwrite-policy=always"]

  "test | build and test" ◉ [hakeExecutable] ∰
    rawSystem hakeExecutable ["--version"]
      >>= checkExitCode

 where buildPath ∷ String
       buildPath = "dist-newstyle"

       hakeExecutable ∷ String
       hakeExecutable =
         if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> "hake.exe"
            | otherwise → buildPath </> "hake"
