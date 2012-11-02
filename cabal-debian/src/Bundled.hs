-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.

{-# LANGUAGE StandaloneDeriving #-}
module Bundled
    ( ghcBuiltIn
    ) where

import qualified Data.Map as Map
import Data.Set (fromList, member)
import Data.Version (Version(..))
import Debian.Relation.ByteString()
import Distribution.Simple.Compiler (Compiler(..), CompilerId(..), CompilerFlavor(..), {-PackageDB(GlobalPackageDB), compilerFlavor-})
import Distribution.Package (PackageIdentifier(..), PackageName(..) {-, Dependency(..)-})

type Bundled = (CompilerFlavor, Version, [PackageIdentifier])

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

ghcBuiltIns :: Compiler -> Bundled
ghcBuiltIns (Compiler {compilerId = CompilerId GHC compilerVersion}) =
    case Map.lookup compilerVersion
             (Map.fromList [ (Version [7,4,1] [], (GHC, Version [7,4,1] [], ghc741BuiltIns))
                           , (Version [7,4,0,20111219] [], (GHC, Version [7,4,0,20111219] [], ghc740BuiltIns))
                           , (Version [7,4,0,20120108] [], (GHC, Version [7,4,0,20120108] [], ghc740BuiltIns))
                           , (Version [7,2,2] [], (GHC, Version [7,2,2] [], ghc721BuiltIns))
                           , (Version [7,2,1] [], (GHC, Version [7,2,1] [], ghc721BuiltIns))
                           , (Version [7,0,4] [], (GHC, Version [7,0,4] [], ghc701BuiltIns))
                           , (Version [7,0,3] [], (GHC, Version [7,0,3] [], ghc701BuiltIns))
                           , (Version [7,0,1] [], (GHC, Version [7,0,1] [], ghc701BuiltIns))
                           , (Version [6,8,3] [], (GHC, Version [6,8,3] [], ghc683BuiltIns))
                           , (Version [6,8,2] [], (GHC, Version [6,8,2] [], ghc682BuiltIns))
                           , (Version [6,8,1] [], (GHC, Version [6,8,1] [], ghc681BuiltIns))
                           , (Version [6,6,1] [], (GHC, Version [6,6,1] [], ghc661BuiltIns))
                           , (Version [6,6] [], (GHC, Version [6,6] [], ghc66BuiltIns)) ]) of
      Nothing -> error $ "cabal-debian: No bundled package list for ghc " ++ show compilerVersion
      Just x -> x
ghcBuiltIns (Compiler {compilerId = _}) = error "ghcBuiltIns: Only GHC is supported"

ghcBuiltIn :: Compiler -> PackageName -> Bool
ghcBuiltIn compiler package =
    Data.Set.member
        package
        (Data.Set.fromList
             (let {- (Just (_, _, xs)) = unsafePerformIO (ghc6BuiltIns compiler) -}
                  (_, _, xs) = ghcBuiltIns compiler in map pkgName xs))

v :: String -> [Int] -> PackageIdentifier
v n x = PackageIdentifier (PackageName n) (Version x [])

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
