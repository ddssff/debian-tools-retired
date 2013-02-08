{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Debian.Debianize.Debianize
    ( cabalDebian
    , callDebianize
    , runDebianize
    , debianize
    , cabalToDebianization
    ) where

import Control.Applicative ((<$>))
import Data.Lens.Lazy (getL, setL, modL)
import Data.List as List (unlines, intercalate, nub)
import Data.Map as Map (lookup)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set as Set (toList)
import Data.Text as Text (Text, unpack)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.Atoms (Atoms, packageDescription, compat, watch, control, copyright, changelog, comments,
                               sourcePriority, sourceSection, debAction, validate, dryRun, debVersion, revision,
                               sourcePackageName, epochMap, extraLibMap, DebAction(..))
import Debian.Debianize.Cabal (inputCabalization, inputCopyright, inputMaintainer)
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), PackageType(..))
import Debian.Debianize.Dependencies (debianName)
import Debian.Debianize.Finalize (finalizeDebianization)
import Debian.Debianize.Goodies (defaultAtoms, watchAtom)
import Debian.Debianize.Options (options, compileArgs)
import Debian.Debianize.Output (validateDebianization, describeDebianization, writeDebianization)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Utility (withCurrentDirectory, foldEmpty)
import Debian.Policy (PackagePriority(Optional), Section(MainSection), getDebhelperCompatLevel, StandardsVersion)
import Debian.Relation (SrcPkgName(..), BinPkgName(BinPkgName), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Debian.Time (getCurrentLocalRFC822Time)
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, unlines)
import System.Console.GetOpt (usageInfo)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Error (catchIOError)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | The main function for the cabal-debian executable.
cabalDebian :: IO ()
cabalDebian =
    compileAllArgs defaultAtoms >>= \ atoms ->
      case getL debAction atoms of
        SubstVar debType -> substvars atoms debType
        Debianize ->  debianize "." atoms
        Usage -> do
          progName <- getProgName
          let info = "Usage: " ++ progName ++ " [FLAGS]\n"
          putStrLn (usageInfo info options)

-- | Run the debianize function with arguments pulled out of the
-- @CABALDEBIAN@ environment variable, with the build directory set to
-- match what dpkg-buildpackage will use later when it uses the
-- resulting debianization.
callDebianize :: [String] -> IO ()
callDebianize args = debianize "." (compileArgs defaultAtoms args)

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

-- | Generate a debianization for the cabal package in a directory
-- using information from the .cabal file and from the options in
-- Flags.  This ignores any existing debianization except for the
-- debian/changelog file.  A new entry changelog is generated, and any
-- entries already there that look older than the new one are
-- preserved.
debianize :: FilePath -> Atoms -> IO ()
debianize top atoms =
    if getL validate atoms
    then validateDebianization top atoms
    else if getL dryRun atoms
         then describeDebianization top atoms >>= putStr . ("Debianization (dry run):\n" ++)
         else writeDebianization top atoms

-- | Given a Flags record, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
cabalToDebianization :: FilePath -> Atoms -> IO Atoms
cabalToDebianization top old =
    do old' <- inputCabalization top old
       let pkgDesc = fromMaybe (error "cabalToDebianization: No package description") (getL packageDescription old')
       date <- getCurrentLocalRFC822Time
       copyright <- withCurrentDirectory top $ inputCopyright pkgDesc
       maint <- inputMaintainer old' >>= maybe (error "Missing value for --maintainer") return
       level <- getDebhelperCompatLevel
       return $ maybe id putStandards (standardsVersion (getL control old')) $
                debianization date copyright maint level (scrub old')
    where
      -- We really don't want to inherit very much information from
      -- the old debianization, so we should do more here.
      scrub = modL control (\ y -> y {binaryPackages = []})

putStandards :: StandardsVersion -> Atoms -> Atoms
putStandards x deb = modL control (\ y -> y {standardsVersion = Just x}) deb

debianization :: String              -- ^ current date
              -> Text                -- ^ copyright
              -> NameAddr            -- ^ maintainer
              -> Int		-- ^ Default standards version
              -> Atoms      -- ^ Debianization specification
              -> Atoms      -- ^ New debianization
debianization date copyright' maint level deb =
    finalizeDebianization $
    modL compat (maybe (Just level) Just) $
    setL sourcePriority (Just Optional) $
    setL sourceSection (Just (MainSection "haskell")) $
    setL watch (Just (watchAtom (pkgName $ Cabal.package $ pkgDesc)))  $
    setL copyright (Just (Right copyright')) $
    versionInfo maint date $
    addExtraLibDependencies $
    -- Do we want to replace the atoms in the old deb, or add these?
    -- Or should we delete even more information from the original,
    -- keeping only the changelog?  Probably the latter.  So this is
    -- somewhat wrong.
    deb
    where
      pkgDesc = fromMaybe (error "debianization") $ getL packageDescription deb

-- | Set the debianization's version info - everything that goes into
-- the new changelog entry, source package name, exact debian version,
-- log comments, maintainer name, revision date.
versionInfo :: NameAddr -> String -> Atoms -> Atoms
versionInfo debianMaintainer date deb =
    modL changelog (const (Just newLog)) $
    modL control (\ y -> y { source = Just sourceName, Debian.maintainer = Just debianMaintainer }) deb
    where
      newLog =
          case getL changelog deb of
            Nothing -> ChangeLog [newEntry]
            Just (ChangeLog oldEntries) ->
                case dropWhile (\ entry -> logVersion entry > logVersion newEntry) oldEntries of
                  -- If the new package version number matches the old, merge the new and existing log entries
                  entry@(Entry {logVersion = d}) : older | d == logVersion newEntry -> ChangeLog (merge entry newEntry : older)
                  -- Otherwise prepend the new entry
                  entries -> ChangeLog (newEntry : entries)
      newEntry = Entry { logPackage = show (pretty sourceName)
                       , logVersion = convertVersion debinfo (pkgVersion pkgId)
                       , logDists = [parseReleaseName "unstable"]
                       , logUrgency = "low"
                       , logComments = List.unlines $ (map (("  * " <>) . List.intercalate "\n    " . map unpack)) (fromMaybe [["Debianization generated by cabal-debian"]] (getL comments deb))
                       , logWho = show (pretty debianMaintainer)
                       , logDate = date }
      -- Get the source package name, either from the SourcePackageName
      -- atom or construct it from the cabal package name.
      sourceName :: SrcPkgName
      sourceName = maybe (debianName deb Source' pkgId) id (getL sourcePackageName deb)
      merge :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
      merge old new =
          old { logComments = logComments old ++ logComments new
              , logDate = date }
      debinfo = maybe (Right (epoch, fromMaybe "" (getL revision deb))) Left (getL debVersion deb)
      epoch = Map.lookup (pkgName pkgId) (getL epochMap deb)
      pkgId = Cabal.package pkgDesc
      pkgDesc = fromMaybe (error "versionInfo: no PackageDescription") $ getL packageDescription deb

-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
convertVersion :: Either DebianVersion (Maybe Int, String) -> Version -> DebianVersion
convertVersion debinfo cabalVersion =
    case debinfo of
      Left override | override >= parseDebianVersion (show (pretty cabalVersion)) -> override
      Left override -> error ("Version from --deb-version (" ++ show (pretty override) ++
                              ") is older than hackage version (" ++ show (pretty cabalVersion) ++
                              "), maybe you need to unpin this package?")
      Right (debianEpoch, debianRevision) ->
          buildDebianVersion debianEpoch
                             (show (pretty cabalVersion))
                             (foldEmpty Nothing Just debianRevision)

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: Atoms -> Atoms
addExtraLibDependencies deb =
    modL control (\ y -> y {binaryPackages = map f (binaryPackages (getL control deb))}) deb
    where
      f :: BinaryDebDescription -> BinaryDebDescription
      f bin
          | debianName deb Development (Cabal.package pkgDesc) == Debian.package bin
              = bin { relations = g (relations bin) }
      f bin = bin
      g :: Debian.PackageRelations -> Debian.PackageRelations
      g rels = rels { Debian.depends = Debian.depends rels ++
                                map anyrel' (concatMap (\ cab -> maybe [BinPkgName ("lib" ++ cab ++ "-dev")] Set.toList (Map.lookup cab (getL extraLibMap deb)))
                                                       (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc)) }
      pkgDesc = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ getL packageDescription deb

anyrel' :: BinPkgName -> [Relation]
anyrel' x = [Rel x Nothing Nothing]

compileAllArgs :: Atoms -> IO Atoms
compileAllArgs atoms0 =
    compileEnvironmentArgs atoms0 >>= compileCommandlineArgs
    where
      compileEnvironmentArgs atoms0 = (compileArgs atoms0 <$> read <$> getEnv "CABALDEBIAN") `catchIOError` const (return atoms0)
      compileCommandlineArgs atoms0 = compileArgs atoms0 <$> getArgs

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True
