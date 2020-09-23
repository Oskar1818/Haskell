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

bindir     = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/bin"
libdir     = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/lib/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0-HAmfwmCSHdU7LSFzuy2tsd-tetris-exe"
dynlibdir  = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/share/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0"
libexecdir = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/libexec/x86_64-osx-ghc-8.6.5/tetris-0.1.0.0"
sysconfdir = "/Users/oskar/Documents/Github/Labs-Haskell/Tetris/Tetris/.stack-work/install/x86_64-osx/bb293da4a3703e2a65b5bb163a4a3da389b2f9c93fe3937669529f8acbf1ebb0/8.6.5/etc"

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
