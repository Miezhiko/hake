{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Hake
  ( hake
  , phony, obj
  , (◉)
  , (#>), (@>), (##>), (@@>)
  , (♯), (♯♯)
  , (∫), (∰)
  , module HakeLib
  ) where

import           Data.IORef
import           Data.Foldable (for_)

import           Control.Monad

import           Hake.Optional

import           Hake.Core              as HakeLib

import           Hake.Helper.Utils      as HakeLib
import           Hake.Helper.Systemd    as HakeLib
import           Hake.Helper.FileSystem as HakeLib
import           Hake.Helper.Syntax     as HakeLib

import           Hake.Lang.C            as HakeLib
import           Hake.Lang.Rust         as HakeLib
import           Hake.Lang.Haskell      as HakeLib
import           Hake.Lang.Idris2       as HakeLib

hake ∷ IO () → IO ()
hake maybeAction = do
  args ← getArgs
  writeIORef phonyArgs args
  maybeAction
  {- HLINT ignore "Redundant multi-way if" -}
  if | "-h" ∈ args ∨ "--help" ∈ args → displayHelp
     | otherwise → do
        myObjects ← readIORef objects
        for_ myObjects $ uncurry compileObj

displayHelp ∷ IO ()
displayHelp = do
  myPhonyActions ← readIORef phonyActions
  case myPhonyActions of
    [] -> return ()
    xs -> do
      putStrLn "HakeScript options:"
      for_ (reverse xs) $ \(r, _, d) →
        putStrLn $ "  " ++ r ++ " :" ++ d

phony :: (Optional1 [String] (String → IO () → IO ()) r) ⇒ r
phony = opt1 gPhony []

gPhony ∷ [String] → String → IO () → IO ()
gPhony [] arg phonyAction = do
  args ← readIORef phonyArgs
  let (an, de) = nameAndDesc arg
  if an ∈ args
    then do phonyAction
            filtered ← removePhonyArg args an
            when (null filtered) exitSuccess
    else do currentPhony ← readIORef phonyActions
            let new = (an, phonyAction, de) : currentPhony
            writeIORef phonyActions new
gPhony deps arg complexPhonyAction = do
  myPhonyArgs ← readIORef phonyArgs
  myPhonyActions ← readIORef phonyActions
  let (an, de) = nameAndDesc arg
  if an ∈ myPhonyArgs
    then do
      myObjects ← readIORef objects
      for_ deps $ \dep → do
        for_ myObjects $ \(file, buildAction) →
          when (dep == file) $
            compileObj file buildAction
        for_ myPhonyActions $ \(rule, phonyAction, _) →
          when (dep == rule) $ compilePhony rule phonyAction
      complexPhonyAction
      filtered ← removePhonyArg myPhonyArgs an
      when (null filtered) exitSuccess
    else let new = (an, complexPhonyAction, de) : myPhonyActions
         in writeIORef phonyActions new

obj :: (Optional1 [String] (FilePath → IO () → IO ()) r) ⇒ r
obj = opt1 gObj []

gObj ∷ [String] → FilePath → IO () → IO ()
gObj [] arg buildAction = do
  currentObjects ← readIORef objects
  currentObjectList ← readIORef objectsList
  let new = (arg, buildAction) : currentObjects
  writeIORef objectsList (arg : currentObjectList)
  writeIORef objects new
gObj deps arg complexBuildAction = do
  myPhonyActions ← readIORef phonyActions
  myObjects      ← readIORef objects
  myObjectList   ← readIORef objectsList
  for_ deps $ \dep → do
    for_ myObjects $ \(file, buildAction) →
      when (dep == file) $
        compileObj file buildAction
    for_ myPhonyActions $ \(rule, phonyAction, _) →
      when (dep == rule) $ compilePhony rule phonyAction
  let new = (arg, complexBuildAction) : myObjects
  writeIORef objectsList (arg : myObjectList)
  writeIORef objects new

-- operators
infixl 5 ◉
-- even >>= will have 1 priority and $ have priority 0
-- to check priority: type ":i >>=" into ghci
infixl 0 ∰, ∫, #>, ##>, @>, @@>, ♯, ♯♯

-- tuple maker
(◉) ∷ String → [String] → (String, [String])
s ◉ ss = (s, ss)

-- Phony operator
(@>) ∷ String → IO () → IO ()
r @> a = phony r a

-- Phony' operator
(@@>) ∷ (String, [String]) → IO () → IO ()
(r, d) @@> a = phony d r a

-- Unicode variant of phony
(∫) ∷ String → IO () → IO ()
r ∫ a = phony r a

-- Unicode variant of phony'
(∰) ∷ (String, [String]) → IO () → IO ()
(r, d) ∰ a = phony d r a

-- Obj operator
(#>) ∷ String → IO () → IO ()
r #> a = obj r a

-- Obj' operator
(##>) ∷ (String, [String]) → IO () → IO ()
(r, d) ##> a = obj d r a

-- Unicode Obj operator
(♯) ∷ FilePath → IO () → IO ()
r ♯ a = obj r a

-- Unicode Obj' operator
(♯♯) ∷ (FilePath, [String]) → IO () → IO ()
(r, d) ♯♯ a = obj d r a
