{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tykeagent_otlp (
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
version = Version [1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/tykgnt-tlp-1.0.0-427a147d/bin"
libdir     = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/tykgnt-tlp-1.0.0-427a147d/lib"
dynlibdir  = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/lib"
datadir    = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/tykgnt-tlp-1.0.0-427a147d/share"
libexecdir = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/tykgnt-tlp-1.0.0-427a147d/libexec"
sysconfdir = "/Users/aditya.ranjan/.cabal/store/ghc-9.4.4/tykgnt-tlp-1.0.0-427a147d/etc"

getBinDir     = catchIO (getEnv "tykeagent_otlp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tykeagent_otlp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tykeagent_otlp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tykeagent_otlp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tykeagent_otlp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tykeagent_otlp_sysconfdir") (\_ -> return sysconfdir)




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
