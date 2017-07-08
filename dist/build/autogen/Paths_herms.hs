module Paths_herms (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [1,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jack/.cabal/bin"
libdir     = "/home/jack/.cabal/lib/x86_64-linux-ghc-7.10.3/herms-1.1.0.0-8cC0knQLYk1KxtEeLSRqX6"
datadir    = "/home/jack/.cabal/share/x86_64-linux-ghc-7.10.3/herms-1.1.0.0"
libexecdir = "/home/jack/.cabal/libexec"
sysconfdir = "/home/jack/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "herms_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "herms_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "herms_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "herms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "herms_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
