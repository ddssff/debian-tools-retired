-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Debian.Debianize.Types.Debianization
    ( Deb(..)
    , Debianization'
    , newDebianization
    , inputDebianization
    ) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.AtomsType (Atoms(atomMap), HasAtoms(..), DebAtomKey(Source), Atoms, defaultAtoms, insertAtom,
                                   DebAtom(DebCompat), setChangeLog, debControl, putDebControl)
import Debian.Debianize.Input (inputSourceDebDescription, inputAtomsFromDirectory)
import Debian.Debianize.Types.DebControl as Debian (SourceDebDescription(..), newSourceDebDescription)
import Debian.Orphans ()
import Debian.Policy (StandardsVersion, parseMaintainer)
import Debian.Relation (SrcPkgName(SrcPkgName))
import Prelude hiding (init, log)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

class Deb deb where
    sourceDebDescription :: deb -> SourceDebDescription
    setSourceDebDescription :: SourceDebDescription -> deb -> deb
    debAtoms :: deb -> Atoms
    setDebAtoms :: Atoms -> deb -> deb

-- | The full debianization.
data Debianization'
    = Debianization'
      { sourceDebDescription_ :: SourceDebDescription
      -- ^ Represents the debian/control file -
      -- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-sourcecontrolfiles>
      , debAtoms_ :: Atoms
      -- ^ Information about the source and binary packages that will
      -- be transformed into values for the fields that represent the
      -- actual debianization files.  Binary values are associated
      -- with a BinPkgName, the Nothing entries in the map represent
      -- source values.
      -- , atoms :: [DebAtom]
      -- ^ All the additional non-manditory debianization information.
      -- It is possible to construct a set with multiple conflicting
      -- values in this set, for example two different DebSourceFormat
      -- constructors, which makes it unclear what the resulting
      -- debianization should be.  Perhaps a clever Eq instance for
      -- DebAtom would help this situation.
      } deriving (Eq, Show)

instance Deb Debianization' where
    sourceDebDescription = sourceDebDescription_
    setSourceDebDescription a b = b {sourceDebDescription_ = a}
    debAtoms = debAtoms_
    setDebAtoms a b = b {debAtoms_ = a}

instance HasAtoms Debianization' where
    getAtoms = getAtoms . debAtoms
    putAtoms ats x = setDebAtoms ((debAtoms x) {atomMap = ats}) x

instance Deb Atoms where
    sourceDebDescription = fromMaybe (error "No Source Deb Description") . debControl
    setSourceDebDescription = putDebControl
    debAtoms = id
    setDebAtoms x _ = x

{-
instance Deb deb => HasAtoms deb where
    getAtoms = atomMap getAtoms . debAtoms
    putAtoms ats x = setDebAtoms ((debAtoms x) {atomMap = ats}) x
-}

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLog -> Int -> StandardsVersion -> Debianization'
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    setChangeLog log $
    insertAtom Source (DebCompat level) $
    Debianization'
      { sourceDebDescription_ = newSourceDebDescription (SrcPkgName (logPackage entry)) (either error id (parseMaintainer (logWho entry))) standards
      , debAtoms_ = defaultAtoms }
newDebianization _ _ _ = error "Invalid changelog"

inputDebianization :: FilePath -> IO Debianization'
inputDebianization top =
    do xs <- Debianization'
               <$> (fst <$> inputSourceDebDescription debian `catchIOError` (\ e -> error ("Failure parsing SourceDebDescription: " ++ show e)))
               -- <*> inputChangeLog debian `catchIOError` (\ e -> error ("Failure parsing changelog: " ++ show e))
               <*> pure (defaultAtoms)
       inputAtomsFromDirectory debian xs `catch` (\ (e :: SomeException) -> error ("Failure parsing atoms: " ++ show e))
    where
      debian = top </> "debian"
