{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Exception (SomeException, try, throw)
import Control.Monad.Trans (lift, liftIO)
import qualified Data.ByteString.Lazy as L
import Data.List (intersperse)
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Cd as Cd
import qualified Debian.AutoBuilder.BuildTarget.Darcs as Darcs
import qualified Debian.AutoBuilder.BuildTarget.DebDir as DebDir
import qualified Debian.AutoBuilder.BuildTarget.Debianize as Debianize
--import qualified Debian.AutoBuilder.BuildTarget.Dir as Dir
import qualified Debian.AutoBuilder.BuildTarget.Hackage as Hackage
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Patch as Patch
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.BuildTarget.Twice as Twice
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Relation (SrcPkgName(..), PkgName(..))
import Debian.Repo (OSImage, rootPath, rootDir, findSourceTree, copySourceTree, SourceTree(dir'), topdir)
import Debian.Repo.Monad (AptIOT)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (CmdSpec(..))
import System.Process.Progress (runProcessF, qPutStrLn, quieter)

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: OSImage -> P.CacheRec -> P.Packages -> AptIOT IO Download
retrieve buildOS cache target =
    (\ x -> qPutStrLn (" " ++ show (P.spec target)) >> quieter 1 x) $
     case P.spec target of
      P.Apt dist package -> Apt.prepare cache target dist (SrcPkgName (PkgName package))
      P.Bzr string -> Bzr.prepare cache target string

      P.Cd dir spec' ->
          retrieve buildOS cache (target {P.spec = spec'}) >>= \ target' ->
          return $ Download { T.package = target
                            , T.getTop = getTop target' </> dir
                            , T.logText = logText target' ++ " (in subdirectory " ++ dir ++ ")"
                            , T.mVersion = Nothing
                            , T.origTarball = Nothing
                            , T.cleanTarget = cleanTarget target'
                            , T.buildWrapper = id
                            }

      P.Darcs uri -> lift (Darcs.prepare cache target uri)

      P.DataFiles base files loc ->
          do base' <- retrieve buildOS cache (target {P.spec = base})
             files' <- retrieve buildOS cache (target {P.spec = files})
             baseTree <- liftIO $ findSourceTree (T.getTop base')
             filesTree <- liftIO $ findSourceTree (T.getTop files')
             _ <- liftIO $ copySourceTree filesTree (dir' baseTree </> loc)
             return base'

      P.DebDir upstream debian ->
          do upstream' <- retrieve buildOS cache (target {P.spec = upstream})
             debian' <- retrieve buildOS cache (target {P.spec = debian})
             DebDir.prepare cache target upstream' debian'
      P.Debianize package ->
          retrieve buildOS cache (target {P.spec = package}) >>=
          Debianize.prepare cache target

      P.Dir path ->
          do tree <- lift (findSourceTree path)
             return $ T.Download { T.package = target
                                 , T.getTop = topdir tree
                                 , T.logText =  "Built from local directory " ++ show (P.spec target)
                                 , T.mVersion = Nothing
                                 , T.origTarball = Nothing
                                 , T.cleanTarget = \ _ -> return ([], 0)
                                 , T.buildWrapper = id
                                 }

      P.Hackage package -> Hackage.prepare cache target package
      P.Hg string -> Hg.prepare cache target string
      P.Patch base patch ->
          retrieve buildOS cache (target {P.spec = base}) >>=
          liftIO . Patch.prepare cache target buildOS patch

      P.Proc spec' ->
          retrieve buildOS cache (target {P.spec = spec'}) >>= \ base ->
          return $ T.Download {
                       T.package = target
                     , T.getTop = T.getTop base
                     , T.logText = T.logText base ++ " (with /proc mounted)"
                     , T.mVersion = Nothing
                     , T.origTarball = Nothing
                     , T.cleanTarget = T.cleanTarget base
                     , T.buildWrapper = withProc buildOS
                     }
      P.Quilt base patches ->
          do base' <- retrieve buildOS cache (target {P.spec = base})
             patches' <- retrieve buildOS cache (target {P.spec = patches})
             Quilt.prepare cache target base' patches'
      P.SourceDeb spec' ->
          retrieve buildOS cache (target {P.spec = spec'}) >>=
          SourceDeb.prepare cache target
      P.Svn uri -> Svn.prepare cache target uri
      P.Tla string -> Tla.prepare cache target string
      P.Twice base -> retrieve buildOS cache (target {P.spec = base}) >>=
                      Twice.prepare target
      P.Uri uri sum -> Uri.prepare cache target uri sum

-- | Perform an IO operation with /proc mounted
withProc :: forall a. OSImage -> IO a -> IO a
withProc buildOS task =
    do createDirectoryIfMissing True dir
       _ <- quieter 1 $ runProcessF id (RawCommand "mount" ["--bind", "/proc", dir]) L.empty
       result <- try task :: IO (Either SomeException a)
       _ <- quieter 1 $ runProcessF id (RawCommand "umount" [dir]) L.empty
       either throw return result
    where
      dir = rootPath (rootDir buildOS) ++ "/proc"

targetDocumentation :: String
targetDocumentation =
    "TARGET TYPES\n\nEach argument to --target describes a technique for obtaining\n" ++
    "the source code used to build a target.  The following target types are available:\n\n" ++
    concat (intersperse "\n\n" $
            map (concat . intersperse "\n  ")
            [ [ "dir:<path> - A target of this form simply uses whatever it finds on"
              , "the local machine at the given path as the debian source tree."
              , "Packages built using this targets are not allowed to be uploaded"
              , "since they include no revision control information." ]
            , Apt.documentation
            , Cd.documentation
            , Darcs.documentation
            , DebDir.documentation
            , Debianize.documentation
            , Hackage.documentation
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])
