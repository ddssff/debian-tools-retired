{-# LANGUAGE ScopedTypeVariables #-}
module Debian.Repo.File
    ( File (File, path, text)
    , Source (LocalPath, RemotePath)
    , Debian.Repo.File.readFile
    ) where

import Control.Applicative
import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.URI (URI)

data Source = LocalPath FilePath | RemotePath URI

-- |A file whose contents have been read into memory.
data File a = File { path :: Source, text :: Failing a }

readFile :: FilePath -> IO (File T.Text)
readFile x = File <$> return (LocalPath x) <*> (try (T.readFile x) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success)

instance Show Source where
    show (LocalPath p) = p
    show (RemotePath uri) = show uri
