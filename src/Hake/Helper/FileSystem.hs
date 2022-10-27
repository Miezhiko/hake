{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module Hake.Helper.FileSystem
  ( copyDir
  , removeDirIfExists
  , removeIfExists
  ) where

import           Control.Exception
import           Data.Foldable     (for_)
import           Prelude.Unicode

import           System.Directory
import           System.FilePath   ((</>))
import           System.IO.Error   (isDoesNotExistError)

removeIfExists ∷ FilePath → IO ()
removeIfExists ζ = removeFile ζ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = return ()
          | otherwise = throwIO ε

removeDirIfExists ∷ FilePath → IO ()
removeDirIfExists δ = removeDirectoryRecursive δ `catch` handleExists
  where handleExists ε
          | isDoesNotExistError ε = return ()
          | otherwise = throwIO ε

copyDir ∷ FilePath -- source
        → FilePath -- destination
        → IO ()
copyDir src dst = do
  createDirectory dst
  content ← getDirectoryContents src
  let xs = filter (∉ [".", ".."]) content
  for_ xs $ \name → let srcPath = src </> name
                        dstPath = dst </> name
      in doesDirectoryExist srcPath >>= \dirExist →
          if dirExist then copyDir srcPath dstPath
                      else copyFile srcPath dstPath
