-- | Some generic operations with specializations to avoid broken Data
-- instances in types like Text and Set.
{-# LANGUAGE  DeriveDataTypeable, RankNTypes, StandaloneDeriving #-}
module Debian.Debianize.Generic
    ( geq
    , gdiff
    , gshow
    ) where

import Prelude hiding (GT)
import Data.Generics (Data, Typeable, GenericQ, toConstr, showConstr, gzipWithQ, extQ, ext1Q, {- ext2Q, Typeable2, -} gmapQ, Constr, dataTypeName, dataTypeOf)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Text as T
import Data.Set as Set (Set, toList, fromList, difference)
import Debian.Debianize.Types.Debianization (Debianization(..), VersionControlSpec, XField, DebAtom, SourceDebAtom(..), BinaryDebAtom(..))
import Debian.Debianize.Utility (showDeps)
import Debian.Relation (Relation, BinPkgName)
import Triplets (mkQ2, extQ2)

deriving instance Typeable Debianization
deriving instance Typeable SourceDebAtom
deriving instance Typeable BinaryDebAtom

deriving instance Data Debianization
deriving instance Data SourceDebAtom
deriving instance Data BinaryDebAtom

-- ext2Q' :: (Data d, Typeable2 t) => (d -> q) -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q) -> d -> q
-- ext2Q' = ext2Q

geq :: GenericQ (GenericQ Bool)
geq x y =
    (geq' `mkQ2` stringEq `extQ2` textEq `extQ2` setEq1 `extQ2` setEq2 `extQ2` setEq3 `extQ2` setEq4 `extQ2` mapEq1) x y
    where
      -- If the specialized eqs don't work, use the generic.  This
      -- will throw an exception if it encounters something with a
      -- NoRep type.
      geq' :: (Data a, Data b) => a -> b -> Bool
      geq' x' y' = (toConstr x' == toConstr y') && and (gzipWithQ geq x' y')
      stringEq :: String -> String -> Bool
      stringEq a b = (a == b)
      textEq :: T.Text -> T.Text -> Bool
      textEq a b = (a == b)
      setEq1 :: Set VersionControlSpec -> Set VersionControlSpec -> Bool
      setEq1 a b = toList a == toList b
      setEq2 :: Set XField -> Set XField -> Bool
      setEq2 a b = toList a == toList b
      setEq3 :: Set SourceDebAtom -> Set SourceDebAtom -> Bool
      setEq3 a b = (a == b)
      setEq4 :: Set BinaryDebAtom -> Set BinaryDebAtom -> Bool
      setEq4 a b = (a == b)
      mapEq1 :: Map BinPkgName (Set BinaryDebAtom) -> Map BinPkgName (Set BinaryDebAtom) -> Bool
      mapEq1 a b = (a == b)

data Diff
    = Diff { stack :: [Constr], expected :: String, actual :: String }
    | SetDiff { stack :: [Constr], expected :: String, missing :: String, extra :: String }
    deriving (Eq, Show)

gdiff :: GenericQ (GenericQ [Diff])
gdiff x y =
    (gdiff' `mkQ2` stringEq `extQ2` textEq `extQ2` setEq1 `extQ2` setEq2 `extQ2` setEq3 `extQ2` setEq4 `extQ2` mapEq1 `extQ2` atomsEq `extQ2` relEq) x y
    where
      -- If the specialized eqs don't work, use the generic.  This
      -- will throw an exception if it encounters something with a
      -- NoRep type.
      gdiff' :: (Data a, Data b) => a -> b -> [Diff]
      gdiff' x' y' =
          if toConstr x' == toConstr y'
          then map (\ diff -> diff {stack = toConstr x' : stack diff}) (concat (gzipWithQ gdiff x' y'))
          else [Diff {stack = [], expected = gshow x', actual = gshow y'}]
      stringEq :: String -> String -> [Diff]
      stringEq a b = if (a == b) then [] else [Diff {stack = [], expected = show a, actual = show b}]
      textEq :: T.Text -> T.Text -> [Diff]
      textEq a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      setEq1 :: Set VersionControlSpec -> Set VersionControlSpec -> [Diff]
      setEq1 a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      setEq2 :: Set XField -> Set XField -> [Diff]
      setEq2 a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      setEq3 :: Set SourceDebAtom -> Set SourceDebAtom -> [Diff]
      setEq3 a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      setEq4 :: Set BinaryDebAtom -> Set BinaryDebAtom -> [Diff]
      setEq4 a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      mapEq1 :: Map BinPkgName (Set BinaryDebAtom) -> Map BinPkgName (Set BinaryDebAtom) -> [Diff]
      mapEq1 a b = if a == b then [] else [Diff {stack = [], expected = show a, actual = show b}]
      atomsEq :: [DebAtom] -> [DebAtom] -> [Diff]
      atomsEq a b = if fromList a == fromList b
                    then []
                    else [SetDiff {stack = [],
                                   expected = show (sort a),
                                   missing = show (Set.difference (Set.fromList a) (Set.fromList b)),
                                   extra = show (Set.difference (Set.fromList b) (Set.fromList a))}]
      relEq :: [[Relation]] -> [[Relation]] -> [Diff]
      relEq a b = if Set.fromList a == Set.fromList b
                  then []
                  else [SetDiff {stack = [],
                                 expected = show [a, b],
                                 missing = showDeps (sort (Set.toList (Set.difference (Set.fromList a) (Set.fromList b)))),
                                 extra = showDeps (sort (Set.toList (Set.difference (Set.fromList b) (Set.fromList a))))}]

{-
gshow' :: Data a => a -> String
gshow' x =
    (gshow `extQ` (show :: T.Text -> String) `extQ` (show :: Maybe T.Text -> String)) x
-}

-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data a => a -> String
gshow x = gshows x ""

-- | Generic shows
gshows :: Data a => a -> ShowS

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
gshows = ( \t ->
                showChar '('
              . (showString . showConstr . toConstr $ t)
              . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)
              . showChar ')'
         ) `extQ` (shows :: String -> ShowS)
           `extQ` ((shows . T.unpack) :: T.Text -> ShowS)
           `ext1Q` (\ s -> gshows (toList s))
