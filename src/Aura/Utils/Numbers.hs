-- A Parser for version numbers.

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

module Aura.Utils.Numbers
    ( Version(..)
    , version ) where

import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Foldable
import qualified Data.Text as T

import Utilities (eitherToMaybe, asInt)
import BasicPrelude

---

-- TODO: Move this to a unified `Types` file?
data Version = Version { unitsOf    :: [Unit]
                       , revisionOf :: Maybe Int }  -- The number after `-`.
               deriving (Eq, Show, Read, Ord)

data Unit = IUnit Int | SUnit String deriving (Eq, Show, Read, Ord)

version :: T.Text -> Maybe Version
version = eitherToMaybe . parse versionNumber ""

versionNumber :: Parser Version
versionNumber = Version <$> units <*> optional revision

units :: Parser [Unit]
units = fold <$> (some (iunit <|> sunit) `sepBy` oneOf ".:_+")

iunit :: Parser Unit
iunit = IUnit . asInt <$> some digitChar

sunit :: Parser Unit
sunit = SUnit <$> some letterChar

revision :: Parser Int
revision = char '-' *> pure asInt <*> some digitChar
