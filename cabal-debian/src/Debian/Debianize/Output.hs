-- | Wrappers around the debianization function to perform various
-- tasks - output, describe, validate a debianization, run an external
-- script to produce a debianization.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( doDebianizeAction
    , runDebianizeScript
    , writeDebianization
    , describeDebianization
    , compareDebianization
    , validateDebianization
    ) where

import Control.Exception as E (throw)
import Control.Monad.State (get, lift)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL)
import Data.Map as Map (elems, toList)
import Data.Maybe (fromMaybe)
import Data.Text as Text (split, Text, unpack)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import qualified Debian.Debianize.Types as D (BinaryDebDescription(package), SourceDebDescription(binaryPackages, source))
import Debian.Debianize.Types (Top(unTop))
import Debian.Debianize.Files (debianizationFileMap)
import Debian.Debianize.Input (inputDebianization)
import qualified Debian.Debianize.Lenses as Lenses (changelog, control, dryRun, validate)
import Debian.Debianize.Monad (DebT, Atoms)
import Debian.Debianize.Options (putEnvironmentArgs)
import Debian.Debianize.Utility (indent, replaceFile, withCurrentDirectory, zipMaps)
import Prelude hiding (unlines, writeFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, getPermissions, Permissions(executable), setPermissions)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | Run the script in @debian/Debianize.hs@ with the given command
-- line arguments.  Returns @True@ if the script exists and succeeds.
-- In this case it may be assumed that a debianization was created (or
-- updated) in the debian subdirectory of the current directory.  In
-- this way we can include a script in a package to produce a
-- customized debianization more sophisticated than the one that would
-- be produced by the cabal-debian executable.  An example is included
-- in the debian subdirectory of this library.
runDebianizeScript :: [String] -> IO Bool
runDebianizeScript args =
    getEnv "HOME" >>= \ home ->
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True ->
          let autobuilderd = "-i.:" ++ home </> ".autobuilder.d" in
          putEnvironmentArgs args >> readProcessWithExitCode "runhaskell" ([autobuilderd, "debian/Debianize.hs"] ++ args) "" >>= \ result ->
          case result of
            (ExitSuccess, _, _) -> return True
            (code, out, err) ->
              error ("runDebianize failed with " ++ show code ++ ":\n stdout: " ++ show out ++"\n stderr: " ++ show err)

-- | Depending on the options in @atoms@, either validate, describe,
-- or write the generated debianization.
doDebianizeAction :: Top -> DebT IO ()
doDebianizeAction top =
    do new <- get
       case () of
         _ | getL Lenses.validate new ->
               do inputDebianization top
                  old <- get
                  return $ validateDebianization old new
         _ | getL Lenses.dryRun new ->
               do inputDebianization top
                  old <- get
                  diff <- lift $ compareDebianization old new
                  lift $ putStr ("Debianization (dry run):\n" ++ diff)
         _ -> writeDebianization top

-- | Write the files of the debianization @d@ to the directory @top@.
writeDebianization :: Top -> DebT IO ()
writeDebianization top =
    do files <- get >>= lift . debianizationFileMap
       lift $ withCurrentDirectory (unTop top) $ mapM_ (uncurry doFile) (Map.toList files)
       lift $ getPermissions (unTop top </> "debian/rules") >>= setPermissions (unTop top </> "debian/rules") . (\ p -> p {executable = True})
    where
      doFile path text =
          do createDirectoryIfMissing True (takeDirectory path)
             replaceFile path (unpack text)

-- | Return a string describing the debianization - a list of file
-- names and their contents in a somewhat human readable format.
describeDebianization :: MonadIO m => DebT m String
describeDebianization =
    get >>= liftIO . debianizationFileMap >>= return . concatMap (\ (path, text) -> path ++ ": " ++ indent " > " (unpack text)) . Map.toList

-- | Compare the old and new debianizations, returning a string
-- describing the differences.
compareDebianization :: Atoms -> Atoms -> IO String
compareDebianization old new =
    do oldFiles <- debianizationFileMap old
       newFiles <- debianizationFileMap new
       return $ concat $ Map.elems $ zipMaps doFile oldFiles newFiles
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Nothing -- Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

-- | Make sure the new debianization matches the existing
-- debianization in several ways - specifically, version number, and
-- the names of the source and binary packages.  Some debian packages
-- come with a skeleton debianization that needs to be filled in, this
-- can be used to make sure the debianization we produce is usable.
validateDebianization :: Atoms -> Atoms -> ()
validateDebianization old new =
    case () of
      _ | oldVersion /= newVersion -> throw (userError ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion)))
        | oldSource /= newSource -> throw (userError ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource)))
        | oldPackages /= newPackages -> throw (userError ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages)))
        | True -> ()
    where
      oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL Lenses.changelog old))))
      newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL Lenses.changelog new))))
      oldSource = D.source . getL Lenses.control $ old
      newSource = D.source . getL Lenses.control $ new
      oldPackages = map D.package . D.binaryPackages . getL Lenses.control $ old
      newPackages = map D.package . D.binaryPackages . getL Lenses.control $ new
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
