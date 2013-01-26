{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Debian.Debianize.Debianize
    ( cabalDebian
    , callDebianize
    , runDebianize
    , debianize
    , compileArgs
    , cabalToDebianization
    , newDebianization
    ) where

import Data.Maybe
import Data.Text (Text)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Atoms (packageDescription, flags, watchAtom, setSourcePriority, setSourceSection, setChangeLog, compilerVersion,
                               cabalFlagAssignments, putCopyright)
import Debian.Debianize.AtomsType (HasAtoms, DebAtomKey(Source), Flags(..), DebAction(..), Atoms, defaultAtoms, insertAtom, DebAtom(DebCompat))
import Debian.Debianize.Cabal (getSimplePackageDescription, inputCopyright, inputMaintainer)
import Debian.Debianize.Combinators (cdbsRules, versionInfo, addExtraLibDependencies,
                                     putStandards, setSourceBinaries)
import Debian.Debianize.Flags (flagOptions, atomOptions)
import Debian.Debianize.Input (inputDebianization)
import Debian.Debianize.Output (outputDebianization)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types.Debianization as Debian (Debianization(..), SourceDebDescription(..), newSourceDebDescription)
import Debian.Debianize.Utility (withCurrentDirectory)
import Debian.Policy (StandardsVersion, PackagePriority(Optional), Section(MainSection), parseMaintainer)
import Debian.Relation (SrcPkgName(SrcPkgName))
import Debian.Time (getCurrentLocalRFC822Time)
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, unlines)
import System.Console.GetOpt (ArgOrder(..), getOpt', OptDescr(..), usageInfo)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (catchIOError)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)

-- | THe main function for the cabal-debian executable
cabalDebian :: IO ()
cabalDebian =
  withFlags defaultAtoms $ \ atoms ->
      case debAction (flags atoms) of
        SubstVar debType -> substvars atoms debType
        Debianize -> withEnvironmentArgs (debianize ".")
        Usage -> do
          progName <- getProgName
          let info = "Usage: " ++ progName ++ " [FLAGS]\n"
          putStrLn (usageInfo info ((flagOptions ++ atomOptions) :: [OptDescr (Atoms -> Atoms)]))

-- | Run the debianize function with arguments pulled out of the
-- @CABALDEBIAN@ environment variable, with the build directory set to
-- match what dpkg-buildpackage will use later when it uses the
-- resulting debianization.
callDebianize :: [String] -> IO ()
callDebianize args = debianize "." args

-- | Try to run the custom script in debian/Debianize.hs to create the
-- debianization.  This can first put some extra arguments into the
-- @CABALDEBIAN@ environment variable.  Often this is used to set the
-- dryRun option by passing @["-n"]@.
runDebianize :: [String] -> IO Bool
runDebianize args =
    doesFileExist "debian/Debianize.hs" >>= \ exists ->
    case exists of
      False -> return False
      True ->
          putEnvironmentArgs args >> readProcessWithExitCode "runhaskell" ("debian/Debianize.hs" : args) "" >>= \ result ->
          case result of
            (ExitSuccess, _, _) -> return True
            (code, out, err) ->
              error ("runDebianize failed with " ++ show code ++ ":\n stdout: " ++ show out ++"\n stderr: " ++ show err)
    where
      -- Insert a value for CABALDEBIAN into the environment that the
      -- withEnvironment* functions above will find and use.  E.g.
      -- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
      putEnvironmentArgs :: [String] -> IO ()
      putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True

-- | Generate a debianization for the cabal package in a directory
-- using information from the .cabal file and from the options in
-- Flags.  This ignores any existing debianization except for the
-- debian/changelog file.  A new entry changelog is generated, and any
-- entries already there that look older than the new one are
-- preserved.
debianize :: FilePath -> [String] -> IO ()
debianize top args =
    do old <- inputDebianization top
       new <- compileEnvironmentArgs (compileArgs old args) >>= cabalToDebianization top
       outputDebianization old new

-- | Given a Flags record, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
cabalToDebianization :: FilePath -> Debianization -> IO Debianization
cabalToDebianization top old =
    do old' <- getSimplePackageDescription (verbosity (flags old)) (compilerVersion old) (cabalFlagAssignments old) top old
       let pkgDesc = fromMaybe (error "cabalToDebianization") (packageDescription old')
       date <- getCurrentLocalRFC822Time
       copyright <- withCurrentDirectory top $ inputCopyright pkgDesc
       maint <- inputMaintainer pkgDesc old' >>= maybe (error "Missing value for --maintainer") return
       let standards = standardsVersion (sourceDebDescription old')
       return $ debianization date copyright maint standards (scrub old')
    where
      -- We really don't want to inherit very much information from
      -- the old debianization, so we should do more here.
      scrub = setSourceBinaries []

debianization :: String              -- ^ current date
              -> Text                -- ^ copyright
              -> NameAddr            -- ^ maintainer
              -> StandardsVersion
              -> Debianization       -- ^ Existing debianization
              -> Debianization       -- ^ New debianization
debianization date copyright' maint standards oldDeb =
    setSourcePriority Optional $
    setSourceSection (MainSection "haskell") $
    -- setSourceBinaries [] $
    watchAtom (pkgName $ Cabal.package $ pkgDesc)  $
    putCopyright (Right copyright') $
    putStandards standards $
    versionInfo maint date $
    addExtraLibDependencies $
    cdbsRules (Cabal.package pkgDesc) $
    -- Do we want to replace the atoms in the old deb, or add these?
    -- Or should we delete even more information from the original,
    -- keeping only the changelog?  Probably the latter.  So this is
    -- somewhat wrong.
    oldDeb
    where
      pkgDesc = fromMaybe (error "debianization") $ packageDescription oldDeb

-- | Create a Debianization based on a changelog entry and a license
-- value.  Uses the currently installed versions of debhelper and
-- debian-policy to set the compatibility levels.
newDebianization :: ChangeLog -> Int -> StandardsVersion -> Debianization
newDebianization (ChangeLog (WhiteSpace {} : _)) _ _ = error "defaultDebianization: Invalid changelog entry"
newDebianization (log@(ChangeLog (entry : _))) level standards =
    setChangeLog log $
    insertAtom Source (DebCompat level) $
    Debianization
      { sourceDebDescription = newSourceDebDescription (SrcPkgName (logPackage entry)) (either error id (parseMaintainer (logWho entry))) standards
      , debAtoms = defaultAtoms }
newDebianization _ _ _ = error "Invalid changelog"

compileEnvironmentArgs :: HasAtoms atoms => atoms -> IO atoms
compileEnvironmentArgs atoms0 =
    getEnv "CABALDEBIAN" >>= return . compileArgs atoms0 . read

-- | Compile the command line arguments into the atoms value and pass
-- to the action.
withFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withFlags def action = getArgs >>= action . compileArgs def

-- | Read the value of $CABALDEBIAN as a list of command line
-- arguments, construct a ('Flags' -> 'Flags') function from them, and
-- compose it with a function that takes a 'Flags' record.
withEnvironmentFlags :: HasAtoms atoms => atoms -> (atoms -> IO a) -> IO a
withEnvironmentFlags atoms0 f =
    (getEnv "CABALDEBIAN" >>= f . compileArgs atoms0 . read) `catchIOError` (\ _ -> f atoms0)

compileArgs :: HasAtoms atoms => atoms -> [String] -> atoms
compileArgs atoms args =
    case getOpt' RequireOrder (flagOptions ++ atomOptions) args of
      (os, [], [], []) -> foldl (flip ($)) atoms os
      (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                    ", Unrecognized: " ++ show unk ++
                                    ", Non-Options: " ++ show non)

-- | Call a function with a list of strings read from the value of
-- $CABALDEBIAN.
withEnvironmentArgs :: ([String] -> IO a) -> IO a
withEnvironmentArgs f =
  (getEnv "CABALDEBIAN" >>= return . read) `catchIOError` handle >>= f
  where
    handle :: IOError -> IO [String]
    handle _ = return []
