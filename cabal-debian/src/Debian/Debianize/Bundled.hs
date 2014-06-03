-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.

{-# LANGUAGE CPP, StandaloneDeriving #-}
module Debian.Debianize.Bundled
    ( ghcBuiltIn
    ) where

import Control.Monad.Trans (MonadIO)
import Data.Function (on)
import Data.List (sortBy)
import Data.Version (Version(..))
import Debian.Relation.ByteString()
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..), {-PackageDB(GlobalPackageDB), compilerFlavor-})
import Distribution.Package (PackageIdentifier(..), PackageName(..) {-, Dependency(..)-})

type Bundled = (Version, [PackageIdentifier])

-- |Return a list of built in packages for the compiler in an environment.
-- ghcBuiltIns :: FilePath -> IO [PackageIdentifier]
-- ghcBuiltIns root =
--     fchroot root (lazyProcess "ghc-pkg" ["list", "--simple-output"] Nothing Nothing empty) >>=
--     return . map parsePackageIdentifier . words . unpack . fst . collectStdout
--     where
--       parsePackageIdentifier s =
--           let (v', n') = break (== '-') (reverse s)
--               (v, n) = (reverse (tail n'), reverse v') in
--           PackageIdentifier (PackageName n) (Version (map read (filter (/= ".") (groupBy (\ a b -> (a == '.') == (b == '.')) v))) [])

ghcBuiltIns :: CompilerId -> Bundled
#if MIN_VERSION_Cabal(1,21,0)
ghcBuiltIns (CompilerId GHC compilerVersion _) =
#else
ghcBuiltIns (CompilerId GHC compilerVersion) =
#endif
    case dropWhile (\ pr -> (fst pr < compilerVersion)) pairs of
      [] -> error $ "cabal-debian: No bundled package list for ghc " ++ show compilerVersion
      x : _ -> x
ghcBuiltIns _ = error "ghcBuiltIns: Only GHC is supported"

pairs :: [Bundled]
pairs = sortBy
          (compare `on` fst)
          ([ (Version [7,8,20140228] [], ghc781BuiltIns)
           , (Version [7,8,20140130] [], ghc781BuiltIns)
           , (Version [7,8,1] [], ghc781BuiltIns)

           , (Version [7,6,3] [], ghc763BuiltIns)
           , (Version [7,6,2] [], ghc762BuiltIns)
           , (Version [7,6,1] [], ghc761BuiltIns)
           , (Version [7,6,1,20121207] [], ghc761BuiltIns)

           , (Version [7,4,1] [], ghc741BuiltIns)
           , (Version [7,4,0,20111219] [], ghc740BuiltIns)
           , (Version [7,4,0,20120108] [], ghc740BuiltIns)
           , (Version [7,2,2] [], ghc721BuiltIns)
           , (Version [7,2,1] [], ghc721BuiltIns)
           , (Version [7,0,4] [], ghc701BuiltIns)
           , (Version [7,0,3] [], ghc701BuiltIns)
           , (Version [7,0,1] [], ghc701BuiltIns)
           , (Version [6,8,3] [], ghc683BuiltIns)
           , (Version [6,8,2] [], ghc682BuiltIns)
           , (Version [6,8,1] [], ghc681BuiltIns)
           , (Version [6,6,1] [], ghc661BuiltIns)
           , (Version [6,6] [], ghc66BuiltIns) ])

ghcBuiltIn :: MonadIO m => CompilerId -> PackageName -> m (Maybe Version)
ghcBuiltIn ghc package = do
  let (_, xs) = ghcBuiltIns ghc
      packageIds = filter (\ p -> pkgName p == package) xs
  case packageIds of
    [] -> return Nothing
    [p] -> return $ Just (pkgVersion p)
    ps -> error $ "Multiple versions of " ++ show package ++ " built into " ++ show ghc ++ ": " ++ show ps

v :: String -> [Int] -> PackageIdentifier
v n x = PackageIdentifier (PackageName n) (Version x [])

ghc781BuiltIns :: [PackageIdentifier]
ghc781BuiltIns = [
    v "array" [0,5,0,0],
    v "base" [4,7,0,0],
    v "binary" [0,7,1,0],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,10,4,0],
    v "Cabal" [1,18,1,3],
    v "containers" [0,5,4,0],
    v "deepseq" [1,3,0,2],
    v "directory" [1,2,0,2],
    v "filepath" [1,3,0,2],
    v "ghc" [7,8,20140130],
    v "ghc-prim" [0,3,1,0],
    v "haskell2010" [1,1,1,1],
    v "haskell98" [2,0,0,3],
    v "hoopl" [3,10,0,0],
    v "hpc" [0,6,0,1],
    v "integer-gmp" [0,5,1,0],
    v "old-locale" [1,0,0,6],
    v "old-time" [1,1,0,2],
    v "pretty" [1,1,1,1],
    v "process" [1,2,0,0],
    v "template-haskell" [2,9,0,0],
    v "time" [1,4,1],
    v "transformers" [0,3,0,0],
    v "unix" [2,7,0,0]
    ]

ghc763BuiltIns :: [PackageIdentifier]
ghc763BuiltIns = [
    v "array" [0,4,0,1],
    v "base" [4,6,0,1],
    v "binary" [0,5,1,1],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,10,0,2],
    v "Cabal" [1,16,0],
    v "containers" [0,5,0,0],
    v "deepseq" [1,3,0,1],
    v "directory" [1,2,0,1],
    v "filepath" [1,3,0,1],
    v "ghc" [7,6,3],
    v "ghc-prim" [0,3,0,0],
    v "haskell2010" [1,1,1,0],
    v "haskell98" [2,0,0,2],
    v "hoopl" [3,9,0,0],
    v "hpc" [0,6,0,0],
    v "integer-gmp" [0,5,0,0],
    v "old-locale" [1,0,0,5],
    v "old-time" [1,1,0,1],
    v "pretty" [1,1,1,0],
    v "process" [1,1,0,2],
    v "template-haskell" [2,8,0,0],
    v "time" [1,4,0,1],
    v "unix" [2,6,0,1]
    ]

-- Removed: rts, extensible-exceptions
ghc762BuiltIns :: [PackageIdentifier]
ghc762BuiltIns = [
    v "array" [0,4,0,1],
    v "base" [4,6,0,1],
    v "binary" [0,5,1,1],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,10,0,2],
    v "Cabal" [1,16,0],
    v "containers" [0,5,0,0],
    v "deepseq" [1,3,0,1],
    v "directory" [1,2,0,1],
    v "filepath" [1,3,0,1],
    v "ghc" [7,6,2],
    v "ghc-prim" [0,3,0,0],
    v "haskell2010" [1,1,1,0],
    v "haskell98" [2,0,0,2],
    v "hoopl" [3,9,0,0],
    v "hpc" [0,6,0,0],
    v "integer-gmp" [0,5,0,0],
    v "old-locale" [1,0,0,5],
    v "old-time" [1,1,0,1],
    v "pretty" [1,1,1,0],
    v "process" [1,1,0,2],
    v "template-haskell" [2,8,0,0],
    v "time" [1,4,0,1],
    v "unix" [2,6,0,1]
    ]

-- Removed: rts, extensible-exceptions
ghc761BuiltIns :: [PackageIdentifier]
ghc761BuiltIns = [
    v "array" [0,4,0,1],
    v "base" [4,6,0,1],
    v "binary" [0,5,1,1],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,10,0,2],
    v "Cabal" [1,16,0],
    v "containers" [0,5,0,0],
    v "deepseq" [1,3,0,1],
    v "directory" [1,2,0,1],
    v "filepath" [1,3,0,1],
    v "ghc" [7,6,1,20121207],
    v "ghc-prim" [0,3,0,0],
    v "haskell2010" [1,1,1,0],
    v "haskell98" [2,0,0,2],
    v "hoopl" [3,9,0,0],
    v "hpc" [0,6,0,0],
    v "integer-gmp" [0,5,0,0],
    v "old-locale" [1,0,0,5],
    v "old-time" [1,1,0,1],
    v "pretty" [1,1,1,0],
    v "process" [1,1,0,2],
    v "template-haskell" [2,8,0,0],
    v "time" [1,4,0,1],
    v "unix" [2,6,0,1]
    ]

-- | Packages bundled with 7.4.0.20111219-2.
ghc741BuiltIns :: [PackageIdentifier]
ghc741BuiltIns = [
    v "Cabal" [1,14,0],
    v "array" [0,4,0,0],
    v "base" [4,5,0,0],
    v "bin-package-db" [0,0,0,0],
    v "binary" [0,5,1,0],
    v "bytestring" [0,9,2,1],
    v "containers" [0,4,2,1],
    v "deepseq" [1,3,0,0],
    v "directory" [1,1,0,2],
    v "extensible-exceptions" [0,1,1,4],
    v "filepath" [1,3,0,0],
    v "ghc" [7,4,1],
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,1,0,1],
    v "haskell98" [2,0,0,1],
    v "hoopl" [3,8,7,2],
    v "hpc" [0,5,1,1],
    v "integer-gmp" [0,4,0,0],
    v "old-locale" [1,0,0,4],
    v "old-time" [1,1,0,0],
    v "pretty" [1,1,1,0],
    v "process" [1,1,0,1], 
    v "rts" [1,0],
    v "template-haskell" [2,7,0,0],
    v "time" [1,4],
    v "unix" [2,5,1,0] ]

-- | Packages bundled with 7.4.0.20111219-2.
ghc740BuiltIns :: [PackageIdentifier]
ghc740BuiltIns = [
    v "Cabal" [1,14,0],
    v "array" [0,4,0,0],
    v "base" [4,5,0,0],
    v "bin-package-db" [0,0,0,0],
    v "binary" [0,5,1,0],
    v "bytestring" [0,9,2,1],
    v "containers" [0,4,2,1],
    v "deepseq" [1,3,0,0],
    v "directory" [1,1,0,2],
    v "extensible-exceptions" [0,1,1,4],
    v "filepath" [1,3,0,0],
    v "ghc" [7,4,0,20111219],
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,1,0,1],
    v "haskell98" [2,0,0,1],
    v "hoopl" [3,8,7,2],
    v "hpc" [0,5,1,1],
    v "integer-gmp" [0,4,0,0],
    v "old-locale" [1,0,0,4],
    v "old-time" [1,1,0,0],
    v "pretty" [1,1,1,0],
    v "process" [1,1,0,1], 
    v "rts" [1,0],
    v "template-haskell" [2,7,0,0],
    v "time" [1,4],
    v "unix" [2,5,1,0] ]

ghc721BuiltIns :: [PackageIdentifier]
ghc721BuiltIns = [
    v "Cabal" [1,12,0],
    v "array" [0,3,0,3],
    v "base" [4,4,0,0],
    v "bin-package-db" [0,0,0,0],
    v "binary" [0,5,0,2],
    v "bytestring" [0,9,2,0],
    v "containers" [0,4,1,0],
    v "directory" [1,1,0,1],
    v "extensible-exceptions" [0,1,1,3],
    v "filepath" [1,2,0,1],
    v "ghc" [7,2,1],
    -- ghc-binary renamed to binary
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,1,0,0],
    v "haskell98" [2,0,0,0],
    v "hoopl" [3,8,7,1], -- new
    v "hpc" [0,5,1,0],
    v "integer-gmp" [0,3,0,0],
    v "old-locale" [1,0,0,3],
    v "old-time" [1,0,0,7],
    v "pretty" [1,1,0,0],
    v "process" [1,1,0,0], 
    -- random removed
    v "rts" [1,0],
    v "template-haskell" [2,6,0,0],
    v "time" [1,2,0,5],
    v "unix" [2,5,0,0] ]

ghc701BuiltIns :: [PackageIdentifier]
ghc701BuiltIns = [
    v "Cabal" [1,10,0,0],
    v "array" [0,3,0,2],
    v "base" [4,3,0,0],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,9,1,8],
    v "containers" [0,4,0,0],
    v "directory" [1,1,0,0],
    v "extensible-exceptions" [0,1,1,2],
    v "filepath" [1,2,0,0],
    v "ghc" [7,0,1],
    v "ghc-binary" [0,5,0,2],
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,0,0,0],
    v "haskell98" [1,1,0,0],
    v "hpc" [0,5,0,6],
    v "integer-gmp" [0,2,0,2],
    v "old-locale" [1,0,0,2],
    v "old-time" [1,0,0,6],
    v "pretty" [1,0,1,2],
    v "process" [1,0,1,4],
    v "random" [1,0,0,3],
    v "rts" [1,0],
    v "template-haskell" [2,5,0,0],
    v "time" [1,2,0,3],
    v "unix" [2,4,1,0]
  ]

ghc683BuiltIns :: [PackageIdentifier]
ghc683BuiltIns = ghc682BuiltIns

ghc682BuiltIns :: [PackageIdentifier]
ghc682BuiltIns = [
    v "Cabal" [1,2,3,0],
    v "array" [0,1,0,0],
    v "base" [3,0,1,0],
    v "bytestring" [0,9,0,1],
    v "containers" [0,1,0,1],
    v "directory" [1,0,0,0],
    v "filepath" [1,1,0,0],
    v "ghc" [6,8,2,0],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "template-haskell" [2,2,0,0],
    v "unix" [2,3,0,0]
    ]

ghc681BuiltIns :: [PackageIdentifier]
ghc681BuiltIns = [
    v "base" [3,0,0,0],
    v "Cabal" [1,2,2,0],
    v "GLUT" [2,1,1,1],
    v "HGL" [3,2,0,0],
    v "HUnit" [1,2,0,0],
    v "OpenAL" [1,3,1,1],
    v "OpenGL" [2,2,1,1],
    v "QuickCheck" [1,1,0,0],
    v "X11" [1,2,3,1],
    v "array" [0,1,0,0],
    v "bytestring" [0,9,0,1],
    v "cgi" [3001,1,5,1],
    v "containers" [0,1,0,0],
    v "directory" [1,0,0,0],
    v "fgl" [5,4,1,1],
    v "filepatch" [1,1,0,0],
    v "ghc" [6,8,1,0],
    v "haskell-src" [1,0,1,1],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "html" [1,0,1,1],
    v "mtl" [1,1,0,0],
    v "network" [2,1,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "parallel" [1,0,0,0],
    v "parsec" [2,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "regex-base" [0,72,0,1],
    v "regex-compat" [0,71,0,1],
    v "regex-posix" [0,72,0,1],
    v "stm" [2,1,1,0],
    v "template-haskell" [2,2,0,0],
    v "time" [1,1,2,0],
    v "unix" [2,2,0,0],
    v "xhtml" [3000,0,2,1]
    ]

ghc661BuiltIns :: [PackageIdentifier]
ghc661BuiltIns = [
    v "base" [2,1,1],
    v "Cabal" [1,1,6,2],
    v "cgi" [3001,1,1],
    v "fgl" [5,4,1],
    v "filepath" [1,0],
    v "ghc" [6,6,1],
    v "GLUT" [2,1,1],
    v "haskell98" [1,0],
    v "haskell-src" [1,0,1],
    v "HGL" [3,1,1],
    v "html" [1,0,1],
    v "HUnit" [1,1,1],
    v "mtl" [1,0,1],
    v "network" [2,0,1],
    v "OpenAL" [1,3,1],
    v "OpenGL" [2,2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0,1],
    v "readline" [1,0],
    v "regex-base" [0,72],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,1],
    v "time" [1,1,1],
    v "unix" [2,1],
    v "X11" [1,2,1],
    v "xhtml" [3000,0,2]
    ]

ghc66BuiltIns :: [PackageIdentifier]
ghc66BuiltIns = [
    v "base" [2,0],
    v "Cabal" [1,1,6],
    v "cgi" [2006,9,6],
    v "fgl" [5,2],
    v "ghc" [6,6],
    v "GLUT" [2,0],
    v "haskell98" [1,0],
    v "haskell-src" [1,0],
    v "HGL" [3,1],
    v "html" [1,0],
    v "HTTP" [2006,7,7],
    v "HUnit" [1,1],
    v "mtl" [1,0],
    v "network" [2,0],
    v "OpenAL" [1,3],
    v "OpenGL" [2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0],
    v "readline" [1,0],
    v "regex-base" [0,71],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,0],
    v "time" [1,0],
    v "unix" [1,0],
    v "X11" [1,1],
    v "xhtml" [2006,9,13]
    ]

-- Script to output a list of the libraries in the ghc package
-- provides line.  This could be run inside the build environment
-- instead of having these hard coded lists.
--
-- apt-cache show ghc \
--   | grep ^Provides: \
--   | cut -d\  -f2-
--   | sed 's/, /\n/g' \
--   | grep libghc- \
--   | cut -d- -f2- \
--   | grep dev$ \
--   | sort -u \
--   | sed 's/-dev//;s/$/",/;s/^/"/'
--
-- base :: Set String
-- base
--   = Data.Set.fromList
--     [ "array",
--       "base",
--       "binary",
--       "bin-package-db",
--       "bytestring",
--       "cabal",
--       "containers",
--       "deepseq",
--       "directory",
--       "extensible-exceptions",
--       "filepath",
--       "ghc-prim",
--       "haskell2010",
--       "haskell98",
--       "hoopl",
--       "hpc",
--       "integer-gmp",
--       "old-locale",
--       "old-time",
--       "pretty",
--       "process",
--       "rts",
--       "template-haskell",
--       "time",
--       "unix" ]
