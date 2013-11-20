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
    , compareDebianization  -- Used in test
    , validateDebianization
    ) where

import Control.Applicative ((<$>))
import Control.Exception as E (throw)
import Control.Monad.State (lift, get)
import Data.Algorithm.Diff.Context (contextDiff)
import Data.Algorithm.Diff.Pretty (prettyDiff)
import Data.Lens.Lazy (getL, modL)
import Data.List as List (unlines, intercalate, nub)
import Data.Map as Map (lookup, toList, elems)
import Data.Maybe
import Data.Monoid (mempty, (<>))
import Data.Set as Set (toList)
import Data.Text as Text (Text, unpack, split)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import qualified Debian.Debianize.ControlFile as D (SourceDebDescription(..), BinaryDebDescription(..), PackageRelations(..), PackageType(..))
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Files2 (debianName)
import Debian.Debianize.Finalize (finalizeDebianization)
import Debian.Debianize.Goodies (watchAtom)
import Debian.Debianize.Input (inputDebianization, inputCabalization, inputLicenseFile, inputMaintainer, inputChangeLog)
import qualified Debian.Debianize.Lenses as Lenses
    (packageDescription, control, copyright, changelog, comments,
     debAction, validate, dryRun, debVersion, revision,
     sourcePackageName, epochMap, extraLibMap, compat, maintainer)
import Debian.Debianize.Monad (Atoms, DebT, execDebT, evalDebM, control, changelog, copyright, watch,
                               sourceSection, sourcePriority, compat, sourcePackageName, maintainer)
import Debian.Debianize.Options (options, compileArgs)
import Debian.Debianize.SubstVars (substvars)
import Debian.Debianize.Types (DebAction(..), Top(Top, unTop))
import Debian.Debianize.Utility (withCurrentDirectory, foldEmpty, replaceFile, zipMaps, indent, maybeRead)
import Debian.Policy (PackagePriority(Optional), Section(MainSection), getDebhelperCompatLevel, parseMaintainer)
import Debian.Relation (SrcPkgName(..), BinPkgName(BinPkgName), Relation(Rel))
import Debian.Release (parseReleaseName)
import Debian.Version (DebianVersion, parseDebianVersion, buildDebianVersion)
import Debian.Time (getCurrentLocalRFC822Time)
import Distribution.License (License(AllRightsReserved))
import Distribution.Package (PackageIdentifier(..), PackageName)
import qualified Distribution.PackageDescription as Cabal
import Prelude hiding (writeFile, unlines)
import System.Console.GetOpt (usageInfo)
import System.Directory (doesFileExist, Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing)
import System.Environment (getArgs, getEnv, getProgName, withArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (catchIOError, tryIOError)
import System.Posix.Env (setEnv)
import System.Process (readProcessWithExitCode)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty))

-- | The main function for the cabal-debian executable.
cabalDebian :: DebT IO () -> IO ()
cabalDebian init =
    -- This picks up the options required to decide what action we are
    -- taking.  Yes, it does get repeated in the call to debianize.
    execDebT (init >> compileEnvironmentArgs >> compileCommandlineArgs) mempty >>= \ atoms ->
    case getL Lenses.debAction atoms of
        SubstVar debType -> substvars atoms debType
        Debianize -> debianize (Top ".") init (return ())
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

-- | Read a value out of the CABALDEBIAN environment variable which is
-- the result of applying show to a [String].
withEnvironmentArgs :: ([String] -> DebT IO a) -> DebT IO a
withEnvironmentArgs f =
    lift (tryIOError (getEnv "CABALDEBIAN")) >>= either (\ _ -> f []) (maybe (f []) f . maybeRead)

compileEnvironmentArgs :: DebT IO ()
compileEnvironmentArgs = withEnvironmentArgs compileArgs

compileCommandlineArgs :: DebT IO ()
compileCommandlineArgs = lift getArgs >>= compileArgs

-- | Compile the given arguments into an Atoms value and run the
-- debianize function.  This is basically equivalent to @cabal-debian
-- --debianize@, except that the command line arguments come from the
-- function parameter.
callDebianize :: [String] -> DebT IO () -> IO ()
callDebianize args init =
    withArgs args (debianize (Top ".") init (return ()))

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
debianize :: Top -> DebT IO () -> DebT IO () -> IO ()
debianize top init customize =
    debianization top init customize >>= \ atoms ->
    if getL Lenses.validate atoms
    then inputDebianization top >>= \ old -> return (validateDebianization old atoms)
    else if getL Lenses.dryRun atoms
         then inputDebianization top >>= \ old -> putStr ("Debianization (dry run):\n" ++ compareDebianization (ensureCopyright old) atoms)
         else writeDebianization top atoms
    where
      ensureCopyright = modL Lenses.copyright (maybe (Just (Left AllRightsReserved)) Just)

-- | Given an Atoms value, get any additional configuration
-- information from the environment, read the cabal package
-- description and possibly the debian/changelog file, then generate
-- and return the new debianization (along with the data directory
-- computed from the cabal package description.)
debianization :: Top -> DebT IO () -> DebT IO () -> IO Atoms
debianization top init customize =
    do atoms <- execDebT (init >> compileEnvironmentArgs >> compileCommandlineArgs >> customize >> inputCabalization top) mempty
       log <- (Just <$> inputChangeLog top) `catchIOError` (\ _ -> return Nothing)
       date <- getCurrentLocalRFC822Time
       maint <- inputMaintainer atoms
       level <- maybe getDebhelperCompatLevel (return . Just) (getL Lenses.compat atoms :: Maybe Int)
       copyright <- withCurrentDirectory (unTop top) $ inputLicenseFile (fromMaybe (error $ "cabalToDebianization: Failed to read cabal file in " ++ unTop top)
                                                                                   (getL Lenses.packageDescription atoms))
       execDebT (debianization' date copyright maint level log) atoms

debianization' :: Monad m =>
                  String              -- ^ current date
               -> Maybe Text          -- ^ copyright
               -> Maybe NameAddr      -- ^ maintainer
               -> Maybe Int	      -- ^ Default standards version
               -> Maybe ChangeLog
               -> DebT m ()
debianization' date copy maint level log =
    do deb <- get
       let pkgDesc = fromMaybe (error "debianization") (getL Lenses.packageDescription deb)
       addExtraLibDependencies
       copyright (case getL Lenses.copyright deb of
                    Just x -> x
                    Nothing -> case copy of
                                 Just x -> Right x
                                 Nothing -> Left (Cabal.license pkgDesc))
       watch (watchAtom (pkgName $ Cabal.package $ pkgDesc))
       -- FIXME - sourceSection and sourcePriority should be settable
       sourceSection (MainSection "haskell")
       sourcePriority Optional
       maybe (return ()) changelog log
       maybe (return ()) compat level
       finalizeChangelog maint date
       finalizeControl
       finalizeDebianization pkgDesc

dropFutureChangelogEntries :: Monad m => DebT m ()
dropFutureChangelogEntries =
    do deb <- get
       ver <- debianVersion
       maybe (return ())
             (\ (ChangeLog entries) -> changelog (ChangeLog (dropWhile (\ entry -> logVersion entry > ver) entries)))
             (getL Lenses.changelog deb)


-- | Combine various bits of information to produce the debian version
-- which will be used for the debian package.  If the override
-- parameter is provided this exact version will be used, but an error
-- will be thrown if that version is unusably old - i.e. older than
-- the cabal version of the package.  Otherwise, the cabal version is
-- combined with the given epoch number and revision string to create
-- a version.
debianVersion :: Monad m => DebT m DebianVersion
debianVersion =
    do deb <- get
       let pkgDesc = fromMaybe (error "versionInfo: no PackageDescription") $ getL Lenses.packageDescription deb
           pkgId = Cabal.package pkgDesc
       epoch <- debianEpoch (pkgName pkgId)
       case getL Lenses.debVersion deb of
         Just override
             | override < parseDebianVersion (show (pretty (pkgVersion pkgId))) ->
                 error ("Version from --deb-version (" ++ show (pretty override) ++
                        ") is older than hackage version (" ++ show (pretty (pkgVersion pkgId)) ++
                        "), maybe you need to unpin this package?")
         Just override -> return override
         Nothing ->
             let rev = foldEmpty Nothing Just (fromMaybe "" (getL Lenses.revision deb))
                 ver = show (pretty (pkgVersion pkgId)) in
             return $ buildDebianVersion epoch ver rev

-- | Return the Debian epoch number assigned to the given cabal
-- package - the 1 in version numbers like 1:3.5-2.
debianEpoch :: Monad m => PackageName -> DebT m (Maybe Int)
debianEpoch name = get >>= return . Map.lookup name . getL Lenses.epochMap

-- | Compute and return the debian source package name, based on the
-- sourcePackageName if it was specified, and constructed from the
-- cabal name otherwise.
sourceName :: Monad m => DebT m SrcPkgName
sourceName =
    do deb <- get
       let pkgDesc = fromMaybe (error "versionInfo: no PackageDescription") $ getL Lenses.packageDescription deb
           pkgId = Cabal.package pkgDesc
           name = maybe (evalDebM (debianName D.Source' pkgId) deb) id (getL Lenses.sourcePackageName deb)
       sourcePackageName name
       return name

finalizeControl :: Monad m => DebT m ()
finalizeControl =
    do Just src <- get >>= return . getL Lenses.sourcePackageName
       Just maint <- get >>= return . getL Lenses.maintainer
       control (\ y -> y { D.source = Just src, D.maintainer = Just maint })

-- | Make sure there is a changelog entry with the version number and
-- source package name implied by the debianization.  This means
-- either adding an entry or modifying the latest entry (if its
-- version number is the exact one in our debianization.)
finalizeChangelog :: Monad m => Maybe NameAddr -> String -> DebT m ()
finalizeChangelog maint date =
    do deb <- get
       ver <- debianVersion
       src <- sourceName
       let (Just (ChangeLog (entry : _))) = getL Lenses.changelog deb
           maint' :: NameAddr
           maint' =
               case maint of
                 Just x -> x
                 Nothing -> case (parseMaintainer (logWho entry)) of
                              Left e -> NameAddr (Just "Invalid Maintainer String") (show e)
                              Right x -> x
       maintainer maint'
       dropFutureChangelogEntries
       let newEntry = Entry { logPackage = show (pretty src)
                            , logVersion = ver
                            , logDists = [parseReleaseName "unstable"]
                            , logUrgency = "low"
                            , logComments = List.unlines $ map (("  * " <>) . List.intercalate "\n    " . map unpack)
                                            (fromMaybe [["Debianization generated by cabal-debian"]] (getL Lenses.comments deb))
                            , logWho = show (pretty maint')
                            , logDate = date }
       case getL Lenses.changelog deb of
         -- If there is already a changelog entry with the exact
         -- version number we need to create, modify it.
         Just (ChangeLog (entry@(Entry {logVersion = d}) : older))
             | d == ver -> changelog (ChangeLog (mergeChangelogEntries entry newEntry : older))
         -- Otherwise create a new log entry
         Just (ChangeLog entries) -> changelog (ChangeLog (newEntry : entries))
         Nothing -> changelog (ChangeLog [newEntry])

mergeChangelogEntries :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
mergeChangelogEntries old new =
    old { logComments = logComments old ++ logComments new
        , logDate = logDate new }

-- | Convert the extraLibs field of the cabal build info into debian
-- binary package names and make them dependendencies of the debian
-- devel package (if there is one.)
addExtraLibDependencies :: Monad m => DebT m ()
addExtraLibDependencies =
    do deb <- get
       control (\ y -> y {D.binaryPackages = map (f deb) (D.binaryPackages (getL Lenses.control deb))})
    where
      f :: Atoms -> D.BinaryDebDescription -> D.BinaryDebDescription
      f deb bin
          | evalDebM (debianName D.Development (Cabal.package (pkgDesc deb))) deb == D.package bin
              = bin { D.relations = g deb (D.relations bin) }
      f _ bin = bin
      g :: Atoms -> D.PackageRelations -> D.PackageRelations
      g deb rels =
          rels { D.depends =
                     concat $
                          [D.depends rels] ++
                          concatMap (\ cab -> maybe [[[Rel (BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]]
                                                    Set.toList
                                                    (Map.lookup cab (getL Lenses.extraLibMap deb)))
                                    (nub $ concatMap Cabal.extraLibs $ Cabal.allBuildInfo $ pkgDesc deb) }
      pkgDesc deb = fromMaybe (error "addExtraLibDependencies: no PackageDescription") $ getL Lenses.packageDescription deb

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
      oldVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL Lenses.changelog old))))
      newVersion = logVersion (head (unChangeLog (fromMaybe (error "Missing changelog") (getL Lenses.changelog new))))
      oldSource = D.source . getL Lenses.control $ old
      newSource = D.source . getL Lenses.control $ new
      oldPackages = map D.package . D.binaryPackages . getL Lenses.control $ old
      newPackages = map D.package . D.binaryPackages . getL Lenses.control $ new
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x
