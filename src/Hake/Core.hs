module Hake.Core
  ( compileObj
  , compilePhony
  , nameAndDesc
  ) where

import           Prelude.Unicode

import           System.Directory
import           System.Exit
import           System.FilePath

import           Data.Foldable     (for_)
import           Data.IORef
import           Data.List.Split
import qualified Data.Map          as M
import qualified Data.Set          as S

import           Control.Monad

import           Hake.Global
import           Hake.StripUtils (strip)

exitWithError ∷ String -> IO ()
exitWithError μ = do putStrLn $ "Error: " ++ μ
                     exitFailure

nameAndDesc ∷ String -> (String, String)
nameAndDesc χ =
  let splt = splitOn "|" χ
  in if length splt > 1
      then (strip (head splt), last splt)
      else (strip χ, "No description")

compilePhony ∷ String -> (IO (), S.Set String) -> IO ()
compilePhony rule (phonyAction, deps) = do
  myPhonyArgs <- readIORef phonyArgs
  when (rule ∈ myPhonyArgs) $
    writeIORef phonyArgs
      $ filter (/= rule) myPhonyArgs
  unless (S.null deps) $ do
    for_ deps $ \dep -> do
      myPhonyActions <- readIORef phonyActions
      myObjects      <- readIORef objects
      for_ (M.lookup dep myObjects) (compileObj False dep)
      for_ (M.lookup dep myPhonyActions) $ \(depPhonyAction, _) ->
        compilePhony dep (depPhonyAction, S.empty)
  phonyAction

-- recursive inner realization
compileObj' ∷ Bool -> [String] -> String -> (IO (), S.Set String) -> IO ()
compileObj' force parents file (buildAction, deps) = do
  currentObjectList <- readIORef objectsSet
  when (S.member file currentObjectList && file ∉ parents) $ do
    currentDir <- getCurrentDirectory
    let fullPath = currentDir </> file
    objExists <- doesFileExist fullPath
    when (not objExists || force) $ do
      unless (S.null deps) $ do
        myPhonyActions <- readIORef phonyActions
        myObjects      <- readIORef objects
        for_ (S.toList deps) $ \dep -> do
          for_ (M.lookup dep myObjects) (compileObj' False (file : parents) dep)
          for_ (M.lookup dep myPhonyActions) $ \(phonyAction, _) ->
            compilePhony dep (phonyAction, S.empty)
      buildAction -- building this file
      objExistsNow <- doesFileExist fullPath
      unless objExistsNow $ exitWithError (fullPath ++ " doesn't exists")

-- unless force will just check if file exists
compileObj ∷ Bool -> String -> (IO (), S.Set String) -> IO ()
compileObj force file bd = compileObj' force [] file bd
