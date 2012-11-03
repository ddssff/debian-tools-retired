{-# LANGUAGE CPP, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- | Support for generating Debianization from Cabal data.

module Distribution.Debian.SubstVars
    ( substvars
    ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Trans (lift)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debian.Control
import qualified Debian.Relation as D
import Distribution.Debian.Config (Flags(depMap), missingDependencies')
import Distribution.Debian.PackageInfo (PackageInfo(..), DebType, debName, debDeps)
import Distribution.Debian.Relations (cabalDependencies)
import Distribution.Debian.Utility
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.Simple.Compiler (CompilerFlavor(..), compilerFlavor, Compiler(..))
import Distribution.Simple.Utils (die)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Text (display)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- |Each cabal package corresponds to a directory <name>-<version>,
-- either in /usr/lib or in /usr/lib/haskell-packages/ghc/lib.
-- In that directory is a compiler subdirectory such as ghc-6.8.2.
-- In the ghc subdirectory is one or two library files of the form
-- libHS<name>-<version>.a and libHS<name>-<version>_p.a.  We can
-- determine the debian package names by running dpkg -S on these
-- names, or examining the /var/lib/dpkg/info/\*.list files.  From
-- these we can determine the source package name, and from that
-- the documentation package name.
substvars :: Flags
          -> PackageDescription		-- ^info from the .cabal file
          -> Compiler   		-- ^compiler details
          -> DebMap
          -> DebType			-- ^The type of deb we want to write substvars for
          -> IO ()
substvars flags pkgDesc compiler debVersions debType =
    libPaths compiler debVersions >>= return . Map.fromList . map (\ p -> (cabalName p, p)) >>= \ cabalPackages ->
    readFile "debian/control" >>= either (error . show) return . parseControl "debian/control" >>= \ control ->
    substvars' flags pkgDesc compiler debVersions debType cabalPackages control

substvars' :: Flags -> PackageDescription -> Compiler -> DebMap -> DebType -> Map.Map String PackageInfo -> Control' String -> IO ()
substvars' flags pkgDesc _compiler _debVersions debType cabalPackages control =
    case (missingBuildDeps, path) of
      -- There should already be a .substvars file produced by dh_haskell_prep,
      -- keep the relations listed there.  They will contain something like this:
      -- libghc-cabal-debian-prof.substvars:
      --    haskell:Depends=ghc-prof (<< 6.8.2-999), ghc-prof (>= 6.8.2), libghc-cabal-debian-dev (= 0.4)
      -- libghc-cabal-debian-dev.substvars:
      --    haskell:Depends=ghc (<< 6.8.2-999), ghc (>= 6.8.2)
      -- haskell-cabal-debian-doc.substvars:
      --    haskell:Depends=ghc-doc, haddock (>= 2.1.0), haddock (<< 2.1.0-999)
      ([], Just path') ->
          do modifyFile path'
               (\ old -> do
                  let new = addDeps old
                  case new /= old of
                    True -> hPutStrLn stderr ("cabal-debian - Updated " ++ show path' ++ ":\n " ++ old ++ "\n   ->\n " ++ new) >> return (Just new)
                    False -> hPutStrLn stderr ("cabal-debian - No updates found for " ++ show path') >> return Nothing)
{-
          do old <- try (readFile path') >>= return . either (\ (_ :: SomeException) -> "") id
             let new = addDeps old
             hPutStrLn stderr (if new /= old
                               then ("cabal-debian - Updated " ++ show path' ++ ":\n " ++ old ++ "\n   ->\n " ++ new)
                               else ("cabal-debian - No updates found for " ++ show path'))
             maybe (return ()) (\ _x -> replaceFile path' new) name
-}
      ([], Nothing) -> return ()
      (missing, _) ->
          die ("These debian packages need to be added to the build dependency list so the required cabal packages are available:\n  " ++ intercalate "\n  " (map (show . D.prettyBinPkgName . fst) missing) ++
               "\nIf this is an obsolete package you may need to withdraw the old versions from the\n" ++
               "upstream repository, and uninstall and purge it from your local system.")
    where
      addDeps old =
          case partition (isPrefixOf "haskell:Depends=") (lines old) of
            ([], other) -> unlines (("haskell:Depends=" ++ showDeps (filterMissing (missingDependencies' flags) deps)) : other)
            (hdeps, more) ->
                case deps of
                  [] -> unlines (hdeps ++ more)
                  _ -> unlines (map (++ (", " ++ showDeps (filterMissing (missingDependencies' flags) deps))) hdeps ++ more)
      path = fmap (\ (D.BinPkgName (D.PkgName x)) -> "debian/" ++ x ++ ".substvars") name
      name = debName control debType
      deps = debDeps debType (depMap flags) cabalPackages pkgDesc control
      -- We must have build dependencies on the profiling and documentation packages
      -- of all the cabal packages.
      missingBuildDeps =
          let requiredDebs =
                  concat (map (\ (Dependency (PackageName name) _) ->
                               case Map.lookup name cabalPackages :: Maybe PackageInfo of
                                 Just info ->
                                     let prof = maybe (devDeb info) Just (profDeb info) in
                                     let doc = docDeb info in
                                     catMaybes [prof, doc]
                                 Nothing -> []) (cabalDependencies (depMap flags) pkgDesc)) in
          filter (not . (`elem` buildDepNames) . fst) requiredDebs
      buildDepNames :: [D.BinPkgName]
      buildDepNames = concat (map (map (\ (D.Rel s _ _) -> s)) buildDeps)
      buildDeps :: D.Relations
      buildDeps = (either (error . show) id . D.parseRelations $ bd) ++ (either (error . show) id . D.parseRelations $ bdi)
      --sourceName = maybe (error "Invalid control file") (\ (Field (_, s)) -> stripWS s) (lookupP "Source" (head (unControl control)))
      bd = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends" . head . unControl $ control
      bdi = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends-Indep" . head . unControl $ control

-- | Collect library information for the packages in the deb
libPaths :: Compiler -> DebMap -> IO [PackageInfo]
libPaths compiler debVersions
    | compilerFlavor compiler == GHC =
        do a <- getDirPaths "/usr/lib"
           b <- getDirPaths "/usr/lib/haskell-packages/ghc/lib"
           -- Build a map from names of installed debs to version numbers
           dpkgFileMap >>= runReaderT (mapM (packageInfo compiler debVersions) (a ++ b)) >>= return . catMaybes
    | True = error $ "Can't handle compiler flavor: " ++ show (compilerFlavor compiler)
    where
      getDirPaths path = try (getDirectoryContents path) >>= return . map (\ x -> (path, x)) . either (\ (_ :: SomeException) -> []) id

-- | Retrieve the package information from the cabal library directory.
packageInfo :: Compiler ->  DebMap -> (FilePath, String) -> ReaderT (Map.Map FilePath (Set.Set D.BinPkgName)) IO (Maybe PackageInfo)
packageInfo compiler debVersions (d, f) =
    case parseNameVersion f of
      Nothing -> return Nothing
      Just (p, v) -> lift (doesDirectoryExist (d </> f </> cdir)) >>= cond (return Nothing) (info (p, v))
    where
      cdir = display (compilerId compiler)
      info (p, v) =
          do dev <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ ".a$")
             prof <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ "_p.a$")
             doc <- debOfFile ("/" ++ p ++ ".haddock$")
             return (Just (PackageInfo { libDir = d
                                       , cabalName = p
                                       , cabalVersion = v
                                       , devDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) dev
                                       , profDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) prof
                                       , docDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) doc }))
      parseNameVersion s =
          case (break (== '-') (reverse s)) of
            (_a, "") -> Nothing
            (a, b) -> Just (reverse (tail b), reverse a)
