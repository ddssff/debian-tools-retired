{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Atoms
    ( DebAtomKey(..)
    , DebAtom(..)
    , HasAtoms(..)
    , insertAtom
    , insertAtoms
    , insertAtoms'
    , lookupAtom
    , lookupAtoms
    , foldAtoms
    , mapAtoms
    , compiler
    , compilerVersion
    , noProfilingLibrary
    , noDocumentationLibrary
    ) where

import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup, insertWith, foldWithKey)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton, fold)
import Data.Text (Text)
import Data.Version (Version)
import Debian.Orphans ()
import Debian.Policy (SourceFormat)
import Debian.Relation (BinPkgName)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init)

data DebAtomKey
    = Source
    | Binary BinPkgName
    deriving (Eq, Ord, Data, Typeable, Show)

-- | The smallest pieces of debhelper information.  Some of these are
-- converted directly into files in the debian directory, others
-- become fragments of those files, and others are first converted
-- into different DebAtom values as new information becomes available.
data DebAtom
    = NoDocumentationLibrary -- replaces haddock
    | NoProfilingLibrary     -- replaces debLibProf
    | Compiler Compiler
    | CompilerVersion Version
      -- ^ Specify the version number of the GHC compiler in the build
      -- environment.  The default is to assume that version is the same
      -- as the one in the environment where cabal-debian is running.
      -- This is used to look up hard coded lists of packages bundled
      -- with the compiler and their version numbers.  (This could
      -- certainly be done in a more beautiful way.)
    | DebSourceFormat SourceFormat                -- ^ Write debian/source/format
    | DebWatch Text                               -- ^ Write debian/watch
    | DHIntermediate FilePath Text                -- ^ Put this text into a file with the given name in the debianization.
    | DebRulesFragment Text                       -- ^ A Fragment of debian/rules
    | Warning Text                                -- ^ A warning to be reported later
    -- From here down are atoms to be associated with a Debian binary package
    | DHApacheSite String FilePath Text           -- ^ Have Apache configure a site using PACKAGE, DOMAIN, LOGDIR, and APACHECONFIGFILE
    | DHInstallLogrotate Text		          -- ^ Add a logrotate file to the binary package
    | DHLink FilePath FilePath          	  -- ^ Create a symbolic link in the binary package
    | DHPostInst Text                   	  -- ^ Script to run after install, should contain #DEBHELPER# line before exit 0
    | DHPostRm Text                     	  -- ^ Script to run after remove, should contain #DEBHELPER# line before exit 0
    | DHPreInst Text                    	  -- ^ Script to run before install, should contain #DEBHELPER# line before exit 0
    | DHPreRm Text                      	  -- ^ Script to run before remove, should contain #DEBHELPER# line before exit 0
    | DHInstall FilePath FilePath       	  -- ^ Install a build file into the binary package
    | DHInstallTo FilePath FilePath     	  -- ^ Install a build file into the binary package at an exact location
    | DHInstallData FilePath FilePath   	  -- ^ DHInstallTo the package's data directory: /usr/share/package-version/
    | DHFile FilePath Text              	  -- ^ Create a file with the given text at the given path
    | DHInstallCabalExec String FilePath  	  -- ^ Install a cabal executable into the binary package
    | DHInstallCabalExecTo String FilePath  	  -- ^ Install a cabal executable into the binary package at an exact location
    | DHInstallDir FilePath             	  -- ^ Create a directory in the binary package
    | DHInstallInit Text                	  -- ^ Add an init.d file to the binary package
    deriving (Eq, Ord, Show)

class HasAtoms atoms where
    getAtoms :: atoms -> Map DebAtomKey (Set DebAtom)
    putAtoms :: Map DebAtomKey (Set DebAtom) -> atoms -> atoms

instance HasAtoms (Map DebAtomKey (Set DebAtom)) where
    getAtoms x = x
    putAtoms _ x = x

lookupAtom :: (HasAtoms atoms, Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Maybe a
lookupAtom mbin from atoms =
    case maxView (lookupAtoms mbin from (getAtoms atoms)) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => DebAtomKey -> (DebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

insertAtom :: HasAtoms atoms => DebAtomKey -> DebAtom -> atoms -> atoms
insertAtom mbin atom x = putAtoms (insertWith union mbin (singleton atom) (getAtoms x)) x

insertAtoms :: HasAtoms atoms => DebAtomKey -> Set DebAtom -> atoms -> atoms
insertAtoms mbin atoms x = putAtoms (insertWith union mbin atoms (getAtoms x)) x

insertAtoms' :: HasAtoms atoms => DebAtomKey -> [DebAtom] -> atoms -> atoms
insertAtoms' mbin atoms x = insertAtoms mbin (fromList atoms) x

foldAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> r -> r) -> r -> atoms -> r
foldAtoms f r0 xs = Map.foldWithKey (\ k s r -> Set.fold (f k) r s) r0 (getAtoms xs)

mapAtoms :: HasAtoms atoms => (DebAtomKey -> DebAtom -> Set DebAtom) -> atoms -> atoms
mapAtoms f xs = foldAtoms (\ k atom xs' -> insertAtoms k (f k atom) xs') (putAtoms mempty xs) (getAtoms xs)

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = fromList . mapMaybe p . toList

compiler :: HasAtoms atoms => atoms -> Maybe Compiler
compiler deb =
    lookupAtom Source fromCompiler deb
    where fromCompiler (Compiler x) = Just x
          fromCompiler _ = Nothing

compilerVersion :: HasAtoms atoms => atoms -> Maybe Version
compilerVersion deb =
    lookupAtom Source from deb
    where from (CompilerVersion x) = Just x
          from _ = Nothing

noProfilingLibrary :: HasAtoms atoms => atoms -> Bool
noProfilingLibrary deb =
    not . Set.null . lookupAtoms Source isNoProfilingLibrary $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = Just NoProfilingLibrary
      isNoProfilingLibrary _ = Nothing

noDocumentationLibrary :: HasAtoms atoms => atoms -> Bool
noDocumentationLibrary deb =
    not . Set.null . lookupAtoms Source isNoDocumentationLibrary $ deb
    where
      isNoDocumentationLibrary NoDocumentationLibrary = Just NoDocumentationLibrary
      isNoDocumentationLibrary _ = Nothing
