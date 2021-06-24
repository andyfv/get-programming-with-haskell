{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_unit5_Type_in_a_context (
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

bindir     = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/bin"
libdir     = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/lib/x86_64-osx-ghc-8.10.4/unit5-Type-in-a-context-0.1.0.0-GIkGDyioQo3Dj1lP4S9uRG-unit5-Type-in-a-context-test"
dynlibdir  = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/share/x86_64-osx-ghc-8.10.4/unit5-Type-in-a-context-0.1.0.0"
libexecdir = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/libexec/x86_64-osx-ghc-8.10.4/unit5-Type-in-a-context-0.1.0.0"
sysconfdir = "/Users/andy/Projects/Haskell/get-programming-with-haskell/unit5-Type-in-a-context/.stack-work/install/x86_64-osx/d583951a711ecec17e909f4c15b10605eff78b0cb3cad968e650bc3f26ab3337/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "unit5_Type_in_a_context_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "unit5_Type_in_a_context_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "unit5_Type_in_a_context_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "unit5_Type_in_a_context_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "unit5_Type_in_a_context_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "unit5_Type_in_a_context_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
