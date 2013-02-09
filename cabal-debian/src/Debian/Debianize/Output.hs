-- | Generate a package Debianization from Cabal data and command line
-- options.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( 
    ) where

import Control.Applicative ((<$>))
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL)
import Data.Map as Map (toList, elems)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, unpack, split)
import Debian.Changes (ChangeLog(ChangeLog), ChangeLogEntry(logVersion))
import Debian.Debianize.Atoms (Atoms, changelog, control)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(source, binaryPackages), BinaryDebDescription(package))
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Goodies (defaultAtoms)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Utility (withCurrentDirectory, replaceFile, zipMaps, indent)
import System.Directory (Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Text.PrettyPrint.ANSI.Leijen (pretty)
