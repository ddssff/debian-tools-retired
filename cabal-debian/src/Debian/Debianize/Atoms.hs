{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | Generate a package Debianization from Cabal data and command line
-- options.

module Debian.Debianize.Atoms
    ( cabalDebian
    , callDebianize
    , runDebianize
    , runDebianize'
    , debianize
    , debianization
    , writeDebianization
    , describeDebianization
    , compareDebianization
    , validateDebianization
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception as E (catch, throw)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL, setL, modL)
import Data.List as List (unlines, intercalate, nub)
import Data.Map as Map (lookup, toList, elems)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Set as Set (toList)
import Data.Text as Text (Text, unpack, split)
import Data.Version (Version)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Debianize.ControlFile as Debian (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), PackageType(..))
import Debian.Debianize.Internal.Dependencies (debianName)
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Finalize (finalizeDebianization)
import Debian.Debianize.Goodies (watchAtom)
import Debian.Debianize.Input (inputDebianization, inputCabalization, inputLicenseFile, inputMaintainer, inputChangeLog)
import Debian.Debianize.Internal.Lenses (packageDescription, compat, watch, control, copyright, changelog, comments,
                                         sourcePriority, sourceSection, debAction, validate, dryRun, debVersion, revision,
                                         sourcePackageName, epochMap, extraLibMap)
import Debian.Debianize.Options (options, compileArgs)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types (DebAction(..), Top(Top, unTop))
import Debian.Debianize.Utility (withCurrentDirectory, foldEmpty, replaceFile, zipMaps, indent, read')
import Debian.DebT (Atoms)
import Debian.Policy (PackagePriority(Optional), Section(MainSection), getDebhelperCompatLevel)
import Debian.Relation (SrcPkgName(..), BinPkgName(BinPkgName), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Debian.Time (getCurrentLocalRFC822Time)
import Distribution.License (License(AllRightsReserved))
import Distribution.Package (PackageIdentifier(..))
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, unlines)
import System.Console.GetOpt (usageInfo)
import System.Directory (doesFileExist, Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing)
import System.Environment (getArgs, getEnv, getProgName, withArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (catchIOError)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | The main function for the cabal-debian executable.
cabalDebian :: Atoms -> IO ()
cabalDebian defaultAtoms =
    compileEnvironmentArgs defaultAtoms >>=
    compileCommandlineArgs >>= \ atoms ->
      case getL debAction atoms of
        SubstVar debType -> substvars atoms debType
        Debianize -> debianize (Top ".") return defaultAtoms
        Usage -> do
          progName <- getProgName
          let info = unlines [ "Typical usage is to cd to the top directory of the package's unpacked source and run: "
                             , ""
                             , "    " ++ progName ++ " --maintainer 'Maintainer Name <maintainer@email>'."
                             , ""
                             , "This will read the package's cabal file and any existing debian/changelog file and"
                             , "deduce what it can about the debianization, then it will create or modify files in"
                             , "the debian subdirectory.  Note that it will not remove any files in debian, and"
                             , "these could affect the operation of the debianization in unknown ways.  For this"
                             , "reason I recommend either using a pristine unpacked directory each time, or else"
                             , "using a revision control system to revert the package to a known state before running."
                             , "The following additional options are available:" ]
          putStrLn (usageInfo info options)

compileEnvironmentArgs :: Atoms -> IO Atoms
compileEnvironmentArgs atoms0 =
    (compileArgs <$> (read' (\ s -> error $ "compileEnvrionmentArgs: " ++ show s) <$> getEnv "CABALDEBIAN") <*> pure atoms0) `catchIOError` const (return atoms0)

compileCommandlineArgs :: Atoms -> IO Atoms
compileCommandlineArgs atoms0 = compileArgs <$> getArgs <*> pure atoms0

-- | Compile the given arguments into an Atoms value and run the
-- debianize function.  This is basically equivalent to @cabal-debian
-- --debianize@, except that the command line arguments come from the
-- function parameter.
callDebianize :: [String] -> Atoms -> IO ()
callDebianize args defaultAtoms =
    withArgs args (debianize (Top ".") return defaultAtoms)

-- | Put an argument list into the @CABALDEBIAN@ environment variable
-- and then run the script in debian/Debianize.hs.  If this exists and
-- succeeds the return value is True, it may be assumed that a
-- debianization was created in the debian subdirectory of the current
-- directory.  This is used to create customized debianizations that
-- are to sophisticated for the command line argument interface
-- available to the cabal-debian executable.
runDebianize :: [String] -> IO Bool
runDebianize args =
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

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True

-- | Call runDebianize with the given working directory.
runDebianize' :: Top -> [String] -> IO Bool
runDebianize' top args = withCurrentDirectory (unTop top) $ runDebianize args

-- | Depending on the options in @atoms@, either validate, describe,
-- or write the generated debianization.
debianize :: Top -> (Atoms -> IO Atoms) -> Atoms -> IO ()
debianize top customize defaultAtoms =
    debianization top customize defaultAtoms >>= \ atoms ->
    if getL validate atoms
    then inputDebianization top >>= \ old -> return (validateDebianization old atoms)
    else if getL dryRun atoms
         then inputDebianization top >>= \ old -> putStr ("Debianization (dry run):\n" ++ compareDebianization (ensureCopyright old) atoms)
         else writeDebianization top atoms
    where
      ensureCopyright = modL copyright (maybe (Just (Left AllRightsReserved)) Just)

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianization :: Top -> (Atoms -> IO Atoms) -> Atoms -> IO Atoms
debianization top customize defaultAtoms =
    do atoms <- compileEnvironmentArgs defaultAtoms >>=
                compileCommandlineArgs >>=
                customize >>=
                inputCabalization top
       log <- (Just <$> inputChangeLog top) `E.catch` (\ (_ :: IOError) -> return Nothing)
       date <- getCurrentLocalRFC822Time
       maint <- inputMaintainer atoms >>= maybe (error "Missing value for --maintainer") return
       level <- getDebhelperCompatLevel
       copyright <- withCurrentDirectory (unTop top) $ inputLicenseFile (fromMaybe (error $ "cabalToDebianization: Failed to read cabal file in " ++ unTop top)
                                                                         (getL packageDescription atoms))
       return $ debianization' date copyright maint level log atoms

debianization' :: String              -- ^ current date
               -> Maybe Text          -- ^ copyright
               -> NameAddr            -- ^ maintainer
               -> Maybe Int	      -- ^ Default standards version
               -> Maybe ChangeLog
               -> Atoms               -- ^ Debianization specification
               -> Atoms               -- ^ New debianization
debianization' date copy maint level log deb =
    finalizeDebianization $
    modL compat (maybe level Just) $
    modL changelog (maybe log Just) $
    setL sourcePriority (Just Optional) $
    setL sourceSection (Just (MainSection "haskell")) $
    setL watch (Just (watchAtom (pkgName $ Cabal.package $ pkgDesc)))  $
    modL copyright (maybe (finalizeCopyright copy) Just) $
    versionInfo maint date $
    addExtraLibDependencies $
    -- Do we want to replace the atoms in the old deb, or add these?
    -- Or should we delete even more information from the original,
    -- keeping only the changelog?  Probably the latter.  So this is
    -- somewhat wrong.
    deb
    where
      pkgDesc = fromMaybe (error "debianization") $ getL packageDescription deb
      finalizeCopyright (Just x) = Just (Right x)
      finalizeCopyright Nothing = Just (Left (Cabal.license pkgDesc))

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
      g rels = rels { Debian.depends =
                        concat $
                          [Debian.depends rels] ++
                          concatMap (\ cab -> maybe [[[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                                    Set.toList
                                                    (Map.lookup cab (getL extraLibMap deb)))
                                    (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc) }
      pkgDesc = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ getL packageDescription deb

-- | Write the files of the debianization @d@ to the directory @top@.
writeDebianization :: Top -> Atoms -> IO ()
writeDebianization top d =
    withCurrentDirectory (unTop top) $
      mapM_ (\ (path, text) ->
                 createDirectoryIfMissing True (takeDirectory path) >>
                 replaceFile path (unpack text))
            (Map.toList (toFileMap d)) >>
      getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})

describeDebianization :: Atoms -> String
describeDebianization atoms =
    concatMap (\ (path, text) -> path ++ ":\n" ++ indent " > " (unpack text)) (Map.toList (toFileMap atoms))

-- | Compare the existing debianization in @top@ to the generated one
-- @new@, returning a string describing the differences.
compareDebianization :: Atoms -> Atoms -> String
compareDebianization old new =
    concat . Map.elems $ zipMaps doFile (toFileMap old) (toFileMap new)
    where
      doFile :: FilePath -> Maybe Text -> Maybe Text -> Maybe String
      doFile path (Just _) Nothing = Just (path ++ ": Deleted\n")
      doFile path Nothing (Just n) = Just (path ++ ": Created\n" ++ indent " | " (unpack n))
      doFile path (Just o) (Just n) =
          if o == n
          then Nothing -- Just (path ++ ": Unchanged\n")
          else Just (show (prettyDiff ("old" </> path) ("new" </> path) (contextDiff 2 (split (== '\n') o) (split (== '\n') n))))
      doFile _path Nothing Nothing = error "Internal error in zipMaps"

-- | Don't change anything, just make sure the new debianization
-- matches the existing debianization in several particulars -
-- specifically, version number, and source and binary package names.
validateDebianization :: Atoms -> Atoms -> ()
validateDebianization old new =
    case () of
      _ | oldVersion /= newVersion -> throw (userError ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion)))
        | oldSource /= newSource -> throw (userError ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource)))
        | oldPackages /= newPackages -> throw (userError ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages)))
        | True -> ()
    where
      oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog old))))
      newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL changelog new))))
      oldSource = source . getL control $ old
      newSource = source . getL control $ new
      oldPackages = map Debian.package . binaryPackages . getL control $ old
      newPackages = map Debian.package . binaryPackages . getL control $ new
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
