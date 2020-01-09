{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quickhull (
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

bindir     = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\quickhull-0.1.0.0-8EU4p8HsjnzBjldM7YgmIi"
dynlibdir  = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\quickhull-0.1.0.0"
libexecdir = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\quickhull-0.1.0.0-8EU4p8HsjnzBjldM7YgmIi\\x86_64-windows-ghc-8.6.5\\quickhull-0.1.0.0"
sysconfdir = "C:\\Users\\Bram\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quickhull_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quickhull_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quickhull_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quickhull_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quickhull_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quickhull_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
