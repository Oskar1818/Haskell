{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tetris (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/bin"
libdir     = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/lib/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0-F59GMPHPZ7G9wdK8JdvWSd"
dynlibdir  = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/share/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0"
libexecdir = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/libexec/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0"
sysconfdir = "/Users/valtermiari/Desktop/Labs-Haskell/Tetris/.stack-work/install/x86_64-osx/f3d22c3f644f73143395f5e88a39561476f8ac961f8bbd9067ed5713d9ae2359/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tetris_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tetris_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tetris_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tetris_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tetris_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tetris_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
