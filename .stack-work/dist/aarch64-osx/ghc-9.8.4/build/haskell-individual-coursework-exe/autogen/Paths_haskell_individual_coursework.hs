{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haskell_individual_coursework (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/bin"
libdir     = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/lib/aarch64-osx-ghc-9.8.4/haskell-individual-coursework-0.1.0.0-BKnaCeggGyD1rJSSAn2ySw-haskell-individual-coursework-exe"
dynlibdir  = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/share/aarch64-osx-ghc-9.8.4/haskell-individual-coursework-0.1.0.0"
libexecdir = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/libexec/aarch64-osx-ghc-9.8.4/haskell-individual-coursework-0.1.0.0"
sysconfdir = "/Users/petros/MSc Computer Science/ECS 713 Functional Programming/Individual/.stack-work/install/aarch64-osx/f670d481bd2d43bb4252bb2f2ab805c4f0c41e0d3db4223f9260d2eb4800ebab/9.8.4/etc"

getBinDir     = catchIO (getEnv "haskell_individual_coursework_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haskell_individual_coursework_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haskell_individual_coursework_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haskell_individual_coursework_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_individual_coursework_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_individual_coursework_sysconfdir") (\_ -> return sysconfdir)



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
