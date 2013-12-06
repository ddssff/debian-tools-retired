{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TupleSections, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.Debianize.Types.Base
    ( Top(..)
    , listElemLens
    , maybeLens
    ) where

import Control.Category ((.))
import Data.Generics (Typeable)
import Data.Lens.Lazy (getL, lens, Lens, setL)
import Data.Maybe (fromMaybe)
import Prelude hiding ((.))

-- | This is a special filepath that represents the top of a directory
-- tree.  For a cabal package this directory would contain the .cabal
-- file, for a debian package it would contain the debian directory.
newtype Top = Top {unTop :: FilePath} deriving (Eq, Ord, Show, Typeable)

listElemLens :: (a -> Bool) -> Lens [a] (Maybe a)
listElemLens p =
    lens lensGet lensPut
    where
      lensGet xs =
          case span (not . p) xs of
            (_, x : _) -> Just x
            _ -> Nothing
      lensPut Nothing xs =
          case span (not . p) xs of
            (pre, _ : post) -> pre ++ post
            _ -> xs
      lensPut (Just x) xs =
          case span (not . p) xs of
            (pre, _ : post) -> pre ++ (x : post)
            _ -> xs ++ [x]

maybeLens :: a -> Lens a b -> Lens (Maybe a) b
maybeLens def l =
    lens (getL l . fromMaybe def)
         (\ a b -> case (a, b) of
                     (_, Nothing) -> Just (setL l a def)
                     (_, Just b') -> Just (setL l a b'))
