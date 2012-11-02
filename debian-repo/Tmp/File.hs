{-# LANGUAGE ScopedTypeVariables #-}
module Tmp.File
    ( File (File, path, text)
    , Source (LocalPath, RemotePath)
    , Tmp.File.readFile
    ) where

import Control.Applicative
import Control.Applicative.Error (Failing(Success, Failure))
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as B
import Network.URI (URI)

data Source = LocalPath FilePath | RemotePath URI

-- |A file whose contents have been read into memory.
data File a = File { path :: Source, text :: Failing a }

readFile :: FilePath -> IO (File B.ByteString)
readFile x = File <$> return (LocalPath x) <*> (try (B.readFile x) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success)

instance Show Source where
    show (LocalPath path) = path
    show (RemotePath uri) = show uri
