-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Debian.Debianize.Types.Debianization
    ( Deb(..)
    , newDebianization
    , inputDebianization
    , modifySourceDebDescription
    ) where

import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe)
import Data.Set as Set (map)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.AtomsType (Atoms, HasAtoms(..), DebAtomKey(Source), Atoms, defaultAtoms, insertAtom,
                                   DebAtom(DebCompat, DebControl), setChangeLog, debControl, modifyAtoms')
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

{-
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
-}

instance Deb Atoms where
    sourceDebDescription = fromMaybe newSourceDebDescription . debControl
    setSourceDebDescription d x = modifySourceDebDescription (const d) x
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
newDebianization :: ChangeLog -> Int -> StandardsVersion -> Atoms
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    setChangeLog log $
    insertAtom Source (DebCompat level) $
    modifySourceDebDescription (\ x -> x { source = Just (SrcPkgName (logPackage entry))
                                         , maintainer = (either error Just (parseMaintainer (logWho entry)))
                                         , standardsVersion = Just standards }) $
    defaultAtoms
newDebianization _ _ _ = error "Invalid changelog"

inputDebianization :: FilePath -> IO Atoms
inputDebianization top =
    do (deb, _) <- inputSourceDebDescription debian `catchIOError` (\ e -> error ("Failure parsing SourceDebDescription: " ++ show e))
       -- Different from snd of above?
       atoms <- inputAtomsFromDirectory debian defaultAtoms `catch` (\ (e :: SomeException) -> error ("Failure parsing atoms: " ++ show e))
       return $ modifySourceDebDescription (const deb) atoms
{-
    do xs <- Debianization'
               <$> (fst <$> )
               -- <*> inputChangeLog debian `catchIOError` (\ e -> error ("Failure parsing changelog: " ++ show e))
               <*> pure (defaultAtoms)
-}
    where
      debian = top </> "debian"

modifySourceDebDescription :: (Deb deb, HasAtoms deb) => (SourceDebDescription -> SourceDebDescription) -> deb -> deb
modifySourceDebDescription f deb =
    modifyAtoms' g (Set.map (\ d -> (Source, DebControl (f d)))) deb
    where
      g Source (DebControl d) = Just d
      g _ _ = Nothing
