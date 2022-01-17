[![Haskell CI](https://github.com/Miezhiko/hake/actions/workflows/haskell.yml/badge.svg?branch=mawa)](https://github.com/Miezhiko/hake/actions/workflows/haskell.yml)

<h1 align="center">
  Hake
  <br>
</h1>

<h4 align="center">Scripty thing designed to write scripts with depending on each other actions.</h4>

<p align="center">
  <a href="#hakescript">HakeScript</a>
  •
  <a href="#usage">Usage</a>
  •
  <a href="#install">Install</a>
  •
  <a href="#build">Build</a>
  •
  <a href="#notes">Notes</a>
</p>

## HakeScript

```haskell
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

import Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cargo ["clean"] >> removeDirIfExists "target"

  amadeusExecutable ♯ do
    cargo <| "build" : buildFlags

  "install | install to system" ◉ [amadeusExecutable] ∰
    cargo <| "install" : buildFlags

  "test | build and test" ◉ [amadeusExecutable] ∰ do
    cargo ["test"]
    rawSystem amadeusExecutable ["--version"]
      >>= checkExitCode

 where buildPath ∷ String
       buildPath = "target/release"

       features ∷ String
       features = "trackers,torch"

       buildFlags ∷ [String]
       buildFlags = ["--release", "--features", features]

       amadeusExecutable ∷ String
       amadeusExecutable =
         {- HLINT ignore "Redundant multi-way if" -}
         if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> "amadeus.exe"
            | otherwise → buildPath </> "amadeus"
```

## Usage

In root of your project you create file `hake.hs` or `hake.lhs`
then you run `hake` or `hake clean` or whatever other options you add to the script,
please use `src/Hake.hs` to learn about all the operators, I will add more examples another time.

## Install

just `cabal install` should install library and executable to userspace

or you can use my `.ebuild` from Overlay and
```bash
emerge hake
```

## Build

simplest:

```bash
stack build
```

complicated:

```
some cabal commands
```

with hake installed:

```bash
hake
```

## Notes

<p align="center">
  <img src="/usage/example.png"/>
</p>
