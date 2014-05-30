module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy as L
import Data.ListLike (ListLike(concat))
import Prelude hiding (length, concat)
import System.Exit
import System.Posix.Files (getFileStatus, fileMode, setFileMode, unionFileModes, ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import System.Process (proc, shell)
import System.Process.Progress (Output(..), readProcessChunks, keepStdout, discardStdout, collectOutputs)
import System.Process.Read.Chunks (readProcessChunks')
import Test.HUnit hiding (path)

main :: IO ()
main =
    do chmod "Tests/Test1.hs"
       chmod "Tests/Test2.hs"
       chmod "Tests/Test4.hs"
       (c,st) <- runTestText putTextToShowS test1 -- (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         _ -> exitFailure

encode :: String -> L.ByteString
encode = L.pack . UTF8.encode

chmod :: FilePath -> IO ()
chmod path =
    getFileStatus "Tests/Test1.hs" >>= \ status ->
    setFileMode path (foldr unionFileModes (fileMode status) [ownerExecuteMode, groupExecuteMode, otherExecuteMode])

test1 :: Test
test1 =
    TestLabel "test1"
      (TestList
       [ TestLabel "pnmfile3" $
         TestCase (do jpg <- L.readFile "Tests/penguin.jpg"
                      pnm <- readProcessChunks (proc "djpeg" []) jpg >>= return . concat . keepStdout :: IO L.ByteString
                      info <- readProcessChunks (proc "pnmfile" []) pnm >>= return . concat . keepStdout :: IO L.ByteString
                      assertEqual "pnmfile3" (encode "stdin:\tPPM raw, 96 by 96  maxval 255\n") info)
       -- , test2
       , TestLabel "readProcessChunks stdout stderr" $
         TestCase (do out <- readProcessChunks (shell "yes | head -10 | while read i; do echo stdout; echo stderr 1>&2; done") L.empty
                      let result = collectOutputs out
                      assertEqual "readProcessChunks stdout stderr" ([ExitSuccess], encode "stdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\n", encode "stderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\n", []) result)
       -- , test3
       , TestLabel "readProcessChunks' stdout stderr" $
         TestCase (do out <- readProcessChunks' (shell "yes | head -10 | while read i; do echo stdout; echo stderr 1>&2; done") L.empty
                      let result = collectOutputs out
                      assertEqual "readProcessChunks' stdout stderr" ([ExitSuccess], encode "stdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\nstdout\n", encode "stderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\nstderr\n", []) result)
{-
       , TestLabel "timed dot test" $
         TestCase (do output <- readModifiedProcess id (ShellCommand "Tests/Test2.hs") "" >>= return . take 10
                      assertEqual "timed dot test" ".........." output)
-}
       ])

-- These are bogus, different versions of gzip give different outputs.  Think of something different.
test2 :: Test
test2 = TestLabel "readProcessChunks gzip" $
        TestCase (do result <- readProcessChunks (shell "gzip -v -f < Tests/penguin.jpg") L.empty
                     assertEqual "readProcessChunks gzip" [Stderr (encode "  2.0%\n"),Result ExitSuccess] (discardStdout result))

test3 :: Test
test3 = TestLabel "readProcessChunks' gzip'" $
        TestCase (do result <- readProcessChunks' (shell "gzip -v -f < Tests/penguin.jpg") L.empty
                     assertEqual "readProcessChunks' gzip" [Stderr (encode "  2.0%\n"),Result ExitSuccess] (discardStdout result))
