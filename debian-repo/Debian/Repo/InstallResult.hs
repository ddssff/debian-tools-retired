{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- |Insert packages into a release.
module Debian.Repo.InstallResult
    ( InstallResult(Ok, Failed, Rejected)
    , isError
    , Problem(..)
    , mergeResults
    , resultToProblems
    , showErrors
    , explainError
    ) where

import Data.List as List (intercalate, map)
import Debian.Release (ReleaseName, releaseName', Section, sectionName')
import Debian.Version.Text ()
import System.IO ()
import System.Posix.Types (FileOffset)

data InstallResult 
    = Ok
    | Failed [Problem]		-- Package can not currently be installed
    | Rejected [Problem]	-- Package can not ever be installed
    deriving (Show, Eq)

data Problem
    = NoSuchRelease ReleaseName
    | NoSuchSection ReleaseName [Section]
    | ShortFile FilePath FileOffset FileOffset
    | LongFile FilePath FileOffset FileOffset
    | MissingFile FilePath
    | BadChecksum FilePath String String
    | OtherProblem String
    deriving (Eq)

instance Show Problem where
    show (NoSuchRelease rel) = "NoSuchRelease  " ++ releaseName' rel
    show (NoSuchSection rel sect) = "NoSuchSection " ++ releaseName' rel ++ " " ++ show (List.map sectionName' sect)
    show (ShortFile path a b) = "ShortFile " ++ path ++ " " ++ show a ++ " " ++ show b
    show (LongFile path a b) = "LongFile " ++ path ++ " " ++ show a ++ " " ++ show b
    show (MissingFile path) = "MissingFile " ++ path
    show (BadChecksum path a b) = "BadChecksum " ++ path ++ " " ++ show a ++ " " ++ show b
    show (OtherProblem s) = "OtherProblem " ++ show s

mergeResults :: [InstallResult] -> InstallResult
mergeResults results =
    doMerge Ok results
    where
      doMerge x [] = x
      doMerge x (Ok : more) = doMerge x more
      doMerge (Rejected p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Rejected p1) (Failed p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Rejected p2 : more) = doMerge (Rejected (p1 ++ p2)) more
      doMerge (Failed p1) (Failed p2 : more) = doMerge (Failed (p1 ++ p2)) more
      doMerge Ok (x : more) = doMerge x more

showErrors :: [InstallResult] -> String
showErrors errors = intercalate "\n" (List.map explainError (concat . List.map resultToProblems $ errors))

resultToProblems :: InstallResult -> [Problem]
resultToProblems Ok = []
resultToProblems (Failed x) = x
resultToProblems (Rejected x) = x

isError :: InstallResult -> Bool
isError Ok = False
isError _ = True

plural :: String -> [a] -> String
plural "do" [_] = "does"
plural "do" _ = "do"

plural "s" [_] = ""
plural "s" _ = "s"

plural _ _ = ""


explainError :: Problem -> String
explainError (NoSuchRelease dist) =
    ("\nThe distribution in the .changes file (" ++ releaseName' dist ++ ") does not exist.  There\n" ++
     "are three common reasons this might happen:\n" ++
     " (1) The value in the latest debian/changelog entry is wrong\n" ++
     " (2) A new release needs to be created in the repository.\n" ++
     "       newdist --root <root> --create-release " ++ releaseName' dist ++ "\n" ++
     " (3) A new alias needs to be created in the repository (typically 'unstable', 'testing', or 'stable'.)\n" ++
     "       newdist --root <root> --create-alias <existing release> " ++ releaseName' dist ++ "\n")
explainError (NoSuchSection dist components) =
    ("\nThe component" ++ plural "s" components ++ " " ++ intercalate ", " (List.map sectionName' components) ++
     " in release " ++ releaseName' dist ++ " " ++
     plural "do" components ++ " not exist.\n" ++
     "either the 'Section' value in debian/control was wrong or the section needs to be created:" ++
     concat (List.map (\ component -> "\n  newdist --root <root> --create-section " ++ releaseName' dist ++ "," ++ sectionName' component) components))
explainError (ShortFile path a b) =
    ("\nThe file " ++ path ++ "\n" ++
     "is shorter than it should be (expected: " ++ show a ++ ", actual: " ++ show b ++ ".)  This usually\n" ++
     "happens while the package is still being uploaded to the repository.")
explainError (LongFile path _ _) =
    ("\nThe file " ++ path ++
     "\nis longer than it should be.  This can happen when the --force-build\n" ++
     "option is used.  In this case the --flush-pool option should help.")
explainError (BadChecksum path _ _) =
    ("\nThe checksum of the file " ++ path ++ "\n" ++
     "is different from the value in the .changes file.\n" ++
     "This can happen when the --force-build option is used.  In this case the\n" ++
     "--flush-pool option should help.  It may also mean a hardware failure.")
explainError other = show other
