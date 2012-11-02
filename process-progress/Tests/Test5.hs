#!/usr/bin/runhaskell

import System.Process (CmdSpec(RawCommand))
import System.Process.Read (readModifiedProcessWithExitCode)

main =
    do (code, out, err) <- readModifiedProcessWithExitCode id (RawCommand "Tests/Test4.hs" []) "abcde"
       putStrLn ("code=" ++ show code ++ "\nout=" ++ show out ++ "\nerr=" ++ show err)
