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
  <a href="#design">Design</a>
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

import Data.List (intercalate)

main ∷ IO ()
main = hake $ do

  "clean | clean the project" ∫
    cargo ["clean"] >> removeDirIfExists targetPath

  "update | update dependencies" ∫ cargo ["update"]

  salieriExecutable ♯
    cargo <| "build" : buildFlagsSalieri False

  amadeusExecutable ◉ [salieriExecutable] ♯♯
    cargo <| "build" : buildFlagsAmadeus False

  "fat | build Amadeus and Salieri with fat LTO" ∫
       cargo <| "build" : buildFlagsSalieri True
    >> cargo <| "build" : buildFlagsAmadeus True

  "install | install to system" ◉ [ "fat" ] ∰
    cargo <| "install" : buildFlagsAmadeus True

  "test | build and test" ◉ [amadeusExecutable] ∰ do
    cargo ["test"]
    cargo ["clippy"]
    rawSystem amadeusExecutable ["--version"]
      >>= checkExitCode

  "restart | restart services" ◉ [ salieriExecutable
                                 , amadeusExecutable ] ∰ do
    systemctl ["restart", appNameSalieri]
    systemctl ["restart", appNameAmadeus]

 where
  appNameSalieri ∷ String
  appNameSalieri = "salieri"

  appNameAmadeus ∷ String
  appNameAmadeus = "amadeus"

  targetPath ∷ FilePath
  targetPath = "target"

  buildPath ∷ FilePath
  buildPath = targetPath </> "release"

  features ∷ [String]
  features = [ "trackers"
             , "torch"
             , "flo" ]

  fatArgs ∷ [String]
  fatArgs = [ "--profile"
            , "fat-release" ]

  buildFlagsSalieri ∷ Bool -> [String]
  buildFlagsSalieri fat =
    let defaultFlags = [ "-p", appNameSalieri, "--release" ]
    in if fat then defaultFlags ++ fatArgs
              else defaultFlags

  buildFlagsAmadeus ∷ Bool -> [String]
  buildFlagsAmadeus fat =
    let defaultFlags = [ "-p", appNameAmadeus
                       , "--release", "--features"
                       , intercalate "," features ]
    in if fat then defaultFlags ++ fatArgs
              else defaultFlags

  salieriExecutable ∷ FilePath
  salieriExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> appNameSalieri ++ ".exe"
       | otherwise → buildPath </> appNameSalieri

  amadeusExecutable ∷ FilePath
  amadeusExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> appNameAmadeus ++ ".exe"
       | otherwise → buildPath </> appNameAmadeus
```

## Usage

In root of your project you create file `hake.hs` or `hake.lhs`
then you run `hake` or `hake clean` or whatever other options you add to the script,
please use `src/Hake.hs` to learn about all the operators, I will add more examples another time.

## Design

Design of this thing is imperative. At first hake will compile (if not compiled) hakescript file (which is basically haskell file with Hake library and wrapper right after main) so we have actual actions compiled and we don't parse them. After that hake will execute compiled hakescript file (aka `hake`) with passing arguments, when hake is executing we gather objects and phony (same terms as in Makefile but with optional syntax sugar and operators) strings into `Data.IORef` structure where we connect actions with obj and phony stuff, next we know what is our target action is (based on passed arguments) and we execute all it's dependencencies (dependencencies of dependencencies first) etc, recursively starting from needed ones without dependencencies. Once action has executed we drop it from IORef structure and check for new action without dependencencies, eventually we execute target action in the very end.

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
