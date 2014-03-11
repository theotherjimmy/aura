-- Core Aura Types

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Core.Types where

import Data.Monoid (Monoid(..))

---

----------
-- ALIASES
----------
type Error    = String
type Pkgbuild = String

-----------
-- PACKAGES
-----------
-- | A package to be installed.
data Package = Package { pkgNameOf        :: String
                       , pkgVersionOf     :: String
                       , pkgDepsOf        :: [Dep]
                       , pkgInstallTypeOf :: InstallType }

-- | A dependency on another package.
data Dep = Dep { depNameOf      :: String
               , depVerDemandOf :: VersionDemand }

-- | The installation method.
data InstallType = Pacman String | Build Buildable

-- | A package to be built manually before installing.
data Buildable = Buildable
    { baseNameOf   :: String
    , pkgbuildOf   :: Pkgbuild
    -- | Did the user select this package, or is it being built as a dep?
    , isExplicit   :: Bool
    -- | Fetch and extract the source code corresponding to the given package.
    , buildScripts :: FilePath     -- ^ Directory in which to place the scripts.
                   -> IO FilePath  -- ^ Path to the extracted scripts.
    }

---------------
-- REPOSITORIES
---------------
-- | A 'Repository' is a place where packages may be fetched from. Multiple
-- repositories can be combined with the 'Data.Monoid' instance.
newtype Repository = Repository
    { repoLookup :: String -> IO (Maybe Package) }

instance Monoid Repository where
    mempty = Repository $ \_ -> return Nothing

    a `mappend` b = Repository $ \s -> do
        mpkg <- repoLookup a s
        case mpkg of
            Nothing -> repoLookup b s
            _       -> return mpkg

-------------------
-- PACKAGE VERSIONS
-------------------
data Version = Version { unitsOf    :: [Unit]
                       , revisionOf :: Maybe Int }  -- The number after `-`.
               deriving (Eq,Show,Ord)

-- TODO: Hide this type from the export?
data Unit = IUnit Int | SUnit String deriving (Eq,Show,Ord)

-- TODO: Change these to hold `Version`s?
data VersionDemand = LessThan String
                   | AtLeast String
                   | MoreThan String
                   | MustBe String
                   | Anything
                     deriving (Eq)

instance Show VersionDemand where
    show (LessThan v) = '<' : v
    show (AtLeast v)  = ">=" ++ v
    show (MoreThan v) = '>' : v
    show (MustBe  v)  = '=' : v
    show Anything     = ""
