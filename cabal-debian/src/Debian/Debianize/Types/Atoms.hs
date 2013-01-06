{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module Debian.Debianize.Types.Atoms
    ( NewDebAtom(..)
    , HasAtoms(..)
    , insertAtom
    , lookupAtom
    , lookupAtoms
    , compiler
    , compilerVersion
    , noProfilingLibrary
    , noDocumentationLibrary
    ) where

import Data.Maybe (mapMaybe)
import Data.Map as Map (Map, lookup, insertWith)
import Data.Set as Set (Set, maxView, toList, fromList, null, empty, union, singleton)
import Data.Version (Version)
import Debian.Orphans ()
import Debian.Orphans ()
import Debian.Relation (BinPkgName)
import Distribution.Simple.Compiler (Compiler)
import Prelude hiding (init)

data NewDebAtom
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
    deriving (Eq, Ord, Show)

class HasAtoms atoms where
    getAtoms :: atoms -> Map (Maybe BinPkgName) (Set NewDebAtom)
    putAtoms :: Map (Maybe BinPkgName) (Set NewDebAtom) -> atoms -> atoms

instance HasAtoms (Map (Maybe BinPkgName) (Set NewDebAtom)) where
    getAtoms x = x
    putAtoms _ x = x

lookupAtom :: HasAtoms atoms => (Show a, Ord a) => Maybe BinPkgName -> (NewDebAtom -> Maybe a) -> atoms -> Maybe a
lookupAtom mbin from atoms =
    case maxView (lookupAtoms mbin from (getAtoms atoms)) of
      Nothing -> Nothing
      Just (x, s) | Set.null s -> Just x
      Just (x, s) -> error $ "lookupAtom - multiple: " ++ show (x : toList s)

lookupAtoms :: HasAtoms atoms => (Show a, Ord a) => Maybe BinPkgName -> (NewDebAtom -> Maybe a) -> atoms -> Set a
lookupAtoms mbin from x = maybe empty (setMapMaybe from) (Map.lookup mbin (getAtoms x))

insertAtom :: HasAtoms atoms => Maybe BinPkgName -> NewDebAtom -> atoms -> atoms
insertAtom mbin atom x = putAtoms (insertWith union mbin (singleton atom) (getAtoms x)) x

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = fromList . mapMaybe p . toList

compiler :: HasAtoms atoms => atoms -> Maybe Compiler
compiler deb =
    lookupAtom Nothing fromCompiler deb
    where fromCompiler (Compiler x) = Just x
          fromCompiler _ = Nothing

compilerVersion :: HasAtoms atoms => atoms -> Maybe Version
compilerVersion deb =
    lookupAtom Nothing from deb
    where from (CompilerVersion x) = Just x
          from _ = Nothing

noProfilingLibrary :: HasAtoms atoms => atoms -> Bool
noProfilingLibrary deb =
    not . Set.null . lookupAtoms Nothing isNoProfilingLibrary $ deb
    where
      isNoProfilingLibrary NoProfilingLibrary = Just NoProfilingLibrary
      isNoProfilingLibrary _ = Nothing

noDocumentationLibrary :: HasAtoms atoms => atoms -> Bool
noDocumentationLibrary deb =
    not . Set.null . lookupAtoms Nothing isNoDocumentationLibrary $ deb
    where
      isNoDocumentationLibrary NoDocumentationLibrary = Just NoDocumentationLibrary
      isNoDocumentationLibrary _ = Nothing
