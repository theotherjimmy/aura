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

module Aura.Pkgbuild.Base where

import Data.Monoid
import qualified Data.Text as T

import Aura.Bash
import Aura.Core
import Aura.Monad.Aura
import Aura.Pkgbuild.Editing
import Filesystem.Path.CurrentOS
import BasicPrelude hiding (FilePath, (<.>), (</>), catch, liftIO)

---

pkgbuildCache :: FilePath
pkgbuildCache = "/var/cache/aura/pkgbuilds"

toFilename :: T.Text -> FilePath
toFilename = (<.> "pb") . fromText

pkgbuildPath :: T.Text -> FilePath
pkgbuildPath p = pkgbuildCache </> toFilename p

trueVersion :: Namespace -> T.Text
trueVersion ns = pkgver <> "-" <> pkgrel
    where pkgver = head $ value ns "pkgver"
          pkgrel = head $ value ns "pkgrel"

-- | Yields the value of the `depends` field.
depends :: Namespace -> [T.Text]
depends = flip value "depends"

makedepends :: Namespace -> [T.Text]
makedepends = flip value "makedepends"

checkdepends :: Namespace -> [T.Text]
checkdepends = flip value "checkdepends"

-- One of my favourite functions in this code base.
pbCustomization :: Buildable -> Aura Buildable
pbCustomization = foldl (>=>) pure [customizepkg, hotEdit]

-- | Package a Buildable, running the customization handler first.
packageBuildable :: Buildable -> Aura Package
packageBuildable b = do
    b' <- pbCustomization b
    ns <- namespace (baseNameOf b') (pkgbuildOf b')
    pure Package
        { pkgNameOf        = baseNameOf b'
        , pkgVersionOf     = trueVersion ns
        , pkgDepsOf        = parseDep <$> foldMap ($ ns)
                             [depends, makedepends, checkdepends]
        , pkgInstallTypeOf = Build b' }
