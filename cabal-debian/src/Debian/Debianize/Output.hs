-- | Generate a package Debianization from Cabal data and command line
-- options.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( writeDebianization
    , describeDebianization
    , compareDebianization
    , validateDebianization
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

-- | Write the files of the debianization @d@ to the directory @top@.
writeDebianization :: FilePath -> Atoms -> IO ()
writeDebianization top d =
    withCurrentDirectory top $
      mapM_ (\ (path, text) ->
                 createDirectoryIfMissing True (takeDirectory path) >>
                 replaceFile path (unpack text))
            (toList (toFileMap d)) >>
      getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})

describeDebianization :: Atoms -> String
describeDebianization atoms =
    concatMap (\ (path, text) -> path ++ ":\n" ++ indent " > " (unpack text)) (toList (toFileMap atoms))

-- | Compare the existing debianization in @top@ to the generated one
-- @new@, returning a string describing the differences.
compareDebianization :: FilePath -> Atoms -> IO String
compareDebianization top new =
    do oldFiles <- toFileMap <$> inputDebianization top defaultAtoms
       let newFiles = toFileMap new
       return $ concat . Map.elems $ zipMaps doFile oldFiles newFiles
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

-- | Don't change anything, just make sure the new debianization
-- matches the existing debianization in several particulars -
-- specifically, version number, and source and binary package names.
validateDebianization :: FilePath -> Atoms -> IO ()
validateDebianization top new =
    do old <- inputDebianization top defaultAtoms
       let oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog old))))
           newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog new))))
           oldSource = source . getL control $ old
           newSource = source . getL control $ new
           oldPackages = map Debian.package . binaryPackages . getL control $ old
           newPackages = map Debian.package . binaryPackages . getL control $ new
       case () of
         _ | oldVersion /= newVersion -> error ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion))
           | oldSource /= newSource -> error ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource))
           | oldPackages /= newPackages -> error ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages))
           | True -> return ()
    where
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
