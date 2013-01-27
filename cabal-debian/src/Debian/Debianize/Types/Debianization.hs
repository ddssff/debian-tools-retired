-- | Preliminary.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Debian.Debianize.Types.Debianization
    ( newDebianization
    , inputDebianization
    ) where

import Control.Exception (SomeException, catch)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.AtomsType (Atoms, DebAtomKey(Source), Atoms, defaultAtoms, insertAtom,
                                   DebAtom(DebCompat), setChangeLog, modifySourceDebDescription)
import Debian.Debianize.Input (inputSourceDebDescription, inputAtomsFromDirectory)
import Debian.Debianize.Types.DebControl as Debian (SourceDebDescription(..))
import Debian.Orphans ()
import Debian.Policy (StandardsVersion, parseMaintainer)
import Debian.Relation (SrcPkgName(SrcPkgName))
import Prelude hiding (init, log)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

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
    where
      debian = top </> "debian"
