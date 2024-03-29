module Hake.Helper.FileSystem
  ( copyDir
  , removeDirIfExists
  , removeIfExists
  ) where

import           Control.Exception
import           Control.Monad     (unless)
import           Data.Foldable     (for_)
import           Prelude.Unicode

import           System.Directory
import           System.FilePath   ((</>))
import           System.IO.Error   (isDoesNotExistError)

removeIfExists ∷ FilePath -> IO ()
removeIfExists ζ = removeFile ζ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = pure ()
          | otherwise = throwIO ε

removeDirIfExists ∷ FilePath -> IO ()
removeDirIfExists δ = removeDirectoryRecursive δ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = pure ()
          | otherwise = throwIO ε

copyDir ∷ FilePath  -- source
        -> FilePath -- destination
        -> IO ()
copyDir src dst = do
  doesDirectoryExist dst >>= \dstDirExist ->
    unless dstDirExist $ createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (∉ [".", ".."]) content
  for_ xs $ \name -> let srcPath = src </> name
                         dstPath = dst </> name
      in doesDirectoryExist srcPath >>= \dirExist ->
          if dirExist then copyDir srcPath dstPath
                      else copyFile srcPath dstPath
