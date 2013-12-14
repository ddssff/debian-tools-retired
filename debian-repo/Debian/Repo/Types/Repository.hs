{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PackageImports, StandaloneDeriving, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.Types.Repository
    ( Repository(LocalRepo, RemoteRepo)
    , fromLocalRepository
    , unLocalRepository
    , unRemoteRepository
    ) where

import Control.Applicative.Error (Failing(Success, Failure), maybeRead)
import Control.Exception (ErrorCall(..), SomeException, toException)
import Control.Monad (filterM, when, unless)
import "MonadCatchIO-mtl" Control.Monad.CatchIO as IO (catch)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (groupBy, partition, sort, isPrefixOf, intercalate)
import Data.Map as Map (Map, insertWith, lookup, insert, fromList, toList, union, empty, mapKeys, mapMaybeWithKey)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (Text, unpack)
import Debian.Changes (ChangesFile(changeInfo), ChangedFileSpec(changedFileSection))
import qualified Debian.Control.Text as T (ControlFunctions(parseControl), Control'(Control), fieldValue, Paragraph, Paragraph')
import Debian.Release (ReleaseName(..), releaseName', Section, sectionName', parseReleaseName, SubSection(section))
import Debian.Repo.Monads.Top (MonadTop, sub)
import Debian.Repo.Sync (rsync)
import Debian.Repo.Types.EnvPath (EnvPath(EnvPath), EnvRoot(EnvRoot), outsidePath)
import Debian.Repo.Types.LocalRepository (LocalRepository(..), repoRoot, repoLayout, Layout(Pool, Flat), prepareLocalRepository)
import Debian.Repo.Types.Release (Release(releaseName), makeReleaseInfo)
import Debian.Repo.Types.RemoteRepository (RemoteRepository(..))
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
import qualified Text.Format as F (Pretty(..))
import qualified Tmp.File as F ( File(..), readFile )
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), vcat, text)
import Text.Regex (matchRegex, mkRegex)
import qualified Tmp.File as F (Source(RemotePath))

--------------------- REPOSITORY -----------------------

-- | The Repository type reprents any instance of the Repo class, so
-- it might be local or remote.
data Repository
    = LocalRepo LocalRepository
    | RemoteRepo RemoteRepository
    deriving (Show, Read)

unRemoteRepository :: Repository -> Maybe RemoteRepository
unRemoteRepository (RemoteRepo x) = Just x
unRemoteRepository _ = Nothing

unLocalRepository :: Repository -> Maybe LocalRepository
unLocalRepository (LocalRepo x) = Just x
unLocalRepository _ = Nothing

instance Ord Repository where
    compare a b = compare (repoKey a) (repoKey b)

instance Eq Repository where
    a == b = compare a b == EQ

instance F.Pretty Repository where
    pretty (LocalRepo r) = text $ outsidePath (repoRoot r)
    pretty (RemoteRepo r) = F.pretty r

instance Repo Repository where
    repoKey (LocalRepo r) = repoKey r
    repoKey (RemoteRepo r) = repoKey r
    repoReleaseInfo (LocalRepo r) = repoReleaseInfo r
    repoReleaseInfo (RemoteRepo r) = repoReleaseInfo r

fromLocalRepository :: LocalRepository -> Repository
fromLocalRepository = LocalRepo
