-- | Generate a package Debianization from Cabal data and command line
-- options.

{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

module Debian.Debianize.Output
    ( outputDebianization
    , describeDebianization
    , writeDebianization
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map as Map (toList)
import Data.Text (Text, unpack)
import Debian.Changes (ChangeLog(ChangeLog), ChangeLogEntry(logVersion))
import Debian.Debianize.AtomsType (Flags(validate, dryRun), flags, changeLog)
import Debian.Debianize.Files (toFileMap)
import Debian.Debianize.Types.Debianization as Debian (Debianization(sourceDebDescription, debAtoms),
                                                       SourceDebDescription(source, binaryPackages), BinaryDebDescription(package))
import Debian.Debianize.Utility (replaceFile, diffFile)
import System.Directory (Permissions(executable), getPermissions, setPermissions, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Text.PrettyPrint.ANSI.Leijen (pretty)

outputDebianization :: Debianization -> Debianization -> IO ()
outputDebianization old new =
       -- It is imperitive that during the time that dpkg-buildpackage
       -- runs the version number in the changelog and the source and
       -- package names in the control file do not change, or the
       -- build will fail.  To ensure this set the validate flag.
       -- This means you probably need to run debianize before
       -- starting dpkg-buildpackage.  However, it is still good to be
       -- able to put the debianize parameters in the Setup file,
       -- rather than storing them apart from the package in the
       -- autobuilder configuration.
       case () of
         _ | validate (flags new) ->
               do let oldVersion = logVersion (head (unChangeLog (changeLog old)))
                      newVersion = logVersion (head (unChangeLog (changeLog new)))
                      oldSource = source . sourceDebDescription $ old
                      newSource = source . sourceDebDescription $ new
                      oldPackages = map Debian.package . binaryPackages . sourceDebDescription $ old
                      newPackages = map Debian.package . binaryPackages . sourceDebDescription $ new
                  case () of
                    _ | oldVersion /= newVersion -> error ("Version mismatch, expected " ++ show (pretty oldVersion) ++ ", found " ++ show (pretty newVersion))
                      | oldSource /= newSource -> error ("Source mismatch, expected " ++ show (pretty oldSource) ++ ", found " ++ show (pretty newSource))
                      | oldPackages /= newPackages -> error ("Package mismatch, expected " ++ show (pretty oldPackages) ++ ", found " ++ show (pretty newPackages))
                      | True -> return ()
           | dryRun (flags new) -> putStrLn "Debianization (dry run):" >> describeDebianization "." new >>= putStr
           | True -> writeDebianization new
    where
      unChangeLog :: ChangeLog -> [ChangeLogEntry]
      unChangeLog (ChangeLog x) = x

-- | Describe a 'Debianization' in relation to one that is written into 
describeDebianization :: FilePath -> Debianization -> IO String
describeDebianization old d =
    mapM (\ (path, text) -> liftIO (doFile path text)) (toList (toFileMap (debAtoms d) (sourceDebDescription d))) >>= return . concat
    where
      doFile :: FilePath -> Text -> IO String
      doFile path text =
          let path' = old </> path in
          doesFileExist path' >>= \ exists ->
              if exists
              then diffFile path' text >>= return . maybe (path ++ ": Unchanged\n") (\ diff -> path ++ ": Modified\n" ++ indent " | " diff)
              else return $ path ++ ": Created\n" ++ indent " | " (unpack text)

writeDebianization :: Debianization -> IO ()
writeDebianization d =
    mapM_ (uncurry doFile) (toList (toFileMap (debAtoms d) (sourceDebDescription d))) >>
    getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
    where
      doFile path text =
          createDirectoryIfMissing True (takeDirectory path) >>
          replaceFile path (unpack text)

indent :: [Char] -> String -> String
indent prefix text = unlines (map (prefix ++) (lines text))
