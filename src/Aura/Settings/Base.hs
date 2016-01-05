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

module Aura.Settings.Base ( Settings(..)
                          , SortScheme(..)
                          , Truncation(..) ) where

import Aura.Languages (Language)

import BasicPrelude hiding (FilePath)
import Shelly (FilePath)
import qualified Data.Text as T

---

data SortScheme = ByVote | Alphabetically deriving (Eq, Show)

data Truncation = None | Head Int | Tail Int deriving (Eq, Show)

-- The global settings as set by the user with command-line flags.
data Settings = Settings { inputOf         :: [T.Text]
                         , pacOptsOf       :: [T.Text]
                         , otherOptsOf     :: [T.Text]
                         , buildUserOf     :: T.Text
                         , langOf          :: Language
                         , pacmanCmdOf     :: FilePath
                         , editorOf        :: T.Text
                         , carchOf         :: T.Text
                         , ignoredPkgsOf   :: [T.Text]
                         , makepkgFlagsOf  :: [String]
                         , buildPathOf     :: FilePath
                         , cachePathOf     :: FilePath
                         , logFilePathOf   :: FilePath
                         , sortSchemeOf    :: SortScheme  -- For `-As`
                         , truncationOf    :: Truncation  -- For `-As`
                         , beQuiet         :: Bool
                         , suppressMakepkg :: Bool
                         , delMakeDeps     :: Bool
                         , diffPkgbuilds   :: Bool
                         , rebuildDevel    :: Bool
                         , mayHotEdit      :: Bool
                         , mustConfirm     :: Bool
                         , neededOnly      :: Bool
                         , useCustomizepkg :: Bool
                         , noPowerPill     :: Bool
                         , keepSource      :: Bool
                         , buildABSDeps    :: Bool
                         , dryRun          :: Bool }
