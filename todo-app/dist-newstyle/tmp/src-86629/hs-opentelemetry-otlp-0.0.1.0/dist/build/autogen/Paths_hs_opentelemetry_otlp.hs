{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hs_opentelemetry_otlp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,0,1,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/hs-pntlmtry-tlp-0.0.1.0-d61ad39b/bin"
libdir     = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/hs-pntlmtry-tlp-0.0.1.0-d61ad39b/lib"
dynlibdir  = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/lib"
datadir    = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/hs-pntlmtry-tlp-0.0.1.0-d61ad39b/share"
libexecdir = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/hs-pntlmtry-tlp-0.0.1.0-d61ad39b/libexec"
sysconfdir = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/hs-pntlmtry-tlp-0.0.1.0-d61ad39b/etc"

getBinDir     = catchIO (getEnv "hs_opentelemetry_otlp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hs_opentelemetry_otlp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hs_opentelemetry_otlp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hs_opentelemetry_otlp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hs_opentelemetry_otlp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hs_opentelemetry_otlp_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
