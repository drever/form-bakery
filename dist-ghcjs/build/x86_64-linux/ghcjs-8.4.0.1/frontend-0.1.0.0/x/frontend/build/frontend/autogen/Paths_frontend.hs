{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_frontend (
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

bindir     = "/Users/johannesdrever/.cabal/bin"
libdir     = "/Users/johannesdrever/.cabal/lib/x86_64-linux-ghcjs-8.4.0.1-ghc8_4_2_20180420/frontend-0.1.0.0-inplace-frontend"
dynlibdir  = "/Users/johannesdrever/.cabal/lib/x86_64-linux-ghcjs-8.4.0.1-ghc8_4_2_20180420"
datadir    = "/Users/johannesdrever/.cabal/share/x86_64-linux-ghcjs-8.4.0.1-ghc8_4_2_20180420/frontend-0.1.0.0"
libexecdir = "/Users/johannesdrever/.cabal/libexec/x86_64-linux-ghcjs-8.4.0.1-ghc8_4_2_20180420/frontend-0.1.0.0"
sysconfdir = "/Users/johannesdrever/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "frontend_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "frontend_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "frontend_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "frontend_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "frontend_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "frontend_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)