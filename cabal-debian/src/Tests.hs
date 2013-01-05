module Main where

import Debian.Cabal.Tests (tests)
import Test.HUnit (runTestTT)

main :: IO ()
main = runTestTT tests >>= putStrLn . show
