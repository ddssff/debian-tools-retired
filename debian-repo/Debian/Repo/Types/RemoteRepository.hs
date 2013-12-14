{-# LANGUAGE FlexibleInstances, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.RemoteRepository
    ( RemoteRepository(..)
    , repoReleaseNames
    ) where

import Control.Applicative.Error (Failing(Success, Failure), maybeRead)
import Control.Exception (ErrorCall(..), SomeException, toException)
import Control.Monad (filterM, when, unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (groupBy, partition, sort, isPrefixOf, intercalate)
import Data.Map as Map (Map, insertWith, lookup, insert, fromList, toList, union, empty)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangesFile(changeInfo), ChangedFileSpec(changedFileSection))
import qualified Debian.Control.Text as T (ControlFunctions(parseControl), Control'(Control), fieldValue, Paragraph, Paragraph')
import Debian.Release (ReleaseName(..), releaseName', Section, sectionName', parseReleaseName, SubSection(section))
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.Types.Release (Release(releaseName), makeReleaseInfo)
import Debian.Repo.Types.Repo (Repo(..), RepoKey(..), compatibilityFile, libraryCompatibilityLevel)
import Debian.Sources ( SliceName(..), DebSource(..), SourceType(..) )
import Debian.URI (URI', fromURI', toURI', uriToString', URI(uriScheme, uriPath), dirFromURI, fileFromURI)
import Debian.UTF8 as Deb (decode)
import Extra.Files (maybeWriteFile)
import Extra.List (partitionM)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified System.Posix.Files as F (getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink,
                                          fileMode, getFileStatus, setFileMode, removeLink)
import System.Process.Progress (quieter, qPutStr, qPutStrLn)
import System.Unix.Directory (removeRecursiveSafely)
import qualified Debian.Repo.Pretty as F (Pretty(..))
import qualified Debian.Repo.File as F ( File(..), readFile )
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat, text)
import Text.Regex (matchRegex, mkRegex)
import qualified Debian.Repo.File as F (Source(RemotePath))

data RemoteRepository
    = RemoteRepository URI' [Release]
    deriving (Read, Show, Eq, Ord)

instance Repo RemoteRepository where
    repoKey (RemoteRepository uri _) = Remote uri
    repoReleaseInfo (RemoteRepository _ info) = info

-- | URI has a bogus show function, which we are using here.
instance F.Pretty URI' where
    pretty = text . show . fromURI'

instance F.Pretty RemoteRepository where
    pretty (RemoteRepository s _) = F.pretty s

-- Nice code to do caching, but I figured out how to fix the old code.

repoReleaseNames :: RemoteRepository -> [ReleaseName]
repoReleaseNames (RemoteRepository _ rels) = map releaseName rels
