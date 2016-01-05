{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

-- Handles all `-C` operations.

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

module Aura.Commands.C
    ( downgradePackages
    , searchCache
    , backupCache
    , cleanCache
    , cleanNotSaved ) where

import Shelly    (FilePath, (</>), rm, cp, fromText, toTextIgnore)
import qualified Data.Text.ICU as Re
import qualified Data.Text.IO as IO
import Data.Text.Read
import Data.Foldable      (traverse_, fold)
import Data.Char          (isDigit)

import Aura.Logo   (raiseCursorBy)
import Aura.Pacman (pacman)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Cache
import Aura.State
import Aura.Utils
import Aura.Core

import Utilities

import qualified Data.Text as T
import BasicPrelude hiding (FilePath, liftIO, (</>))

---

-- | Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: [T.Text] -> [T.Text] -> Aura ()
downgradePackages _ []         = pure ()
downgradePackages pacOpts pkgs = asks cachePathOf >>= \cachePath -> do
  reals <- pkgsInCache pkgs
  reportBadDowngradePkgs (pkgs \\ reals)
  unless (null reals) $ do
    cache   <- cacheContents cachePath
    choices <- traverse (getDowngradeChoice cache) reals
    pacman $ "-U" : pacOpts <> ((toTextIgnore . (cachePath </>) . fromText) <$> choices)

getDowngradeChoice :: Cache -> T.Text -> Aura T.Text
getDowngradeChoice cache pkg = do
  let choices = getChoicesFromCache cache pkg
  notify $ getDowngradeChoice_1 pkg
  liftIO $ getSelection choices

getChoicesFromCache :: Cache -> T.Text -> [T.Text]
getChoicesFromCache cache pkg = sort choices
    where choices = filter match $ map toTextIgnore $ allFilenames cache
          patt    = Re.regex [] $ "^" <> pkg <> "-[0-9:.]+-[1-9]+-(x86_64|i686|any)[.]pkg[.]tar[.]xz$"
          match   = isJust . Re.find patt

-- `[]` as input yields the contents of the entire cache.
searchCache :: [T.Text] -> Aura ()
searchCache r = cacheMatches r >>= liftIO . traverse_ (IO.putStrLn . toTextIgnore) . sortPkgs

-- The destination folder must already exist for the back-up to begin.
backupCache :: [FilePath] -> Aura ()
backupCache []      = scoldAndFail backupCache_1
backupCache (dir:_) = do
  exists' <- liftShelly $ exists dir
  if not exists'
     then scoldAndFail backupCache_3
     else confirmBackup dir >>= backup dir

confirmBackup :: FilePath -> Aura Cache
confirmBackup dir = do
  cache <- ask >>= cacheContents . cachePathOf
  notify $ backupCache_4 dir
  notify . backupCache_5 . size $ cache
  okay <- optionalPrompt backupCache_6
  if not okay
     then scoldAndFail backupCache_7
     else pure cache

backup :: FilePath -> Cache -> Aura ()
backup dir cache = do
  notify backupCache_8
  liftIO $ putStrLn ""  -- So that the cursor can rise at first.
  copyAndNotify dir (allFilenames cache) 1

-- Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: FilePath -> [FilePath] -> Int -> Aura ()
copyAndNotify _ [] _       = pure ()
copyAndNotify dir (p:ps) n = asks cachePathOf >>= \cachePath -> do
  liftIO $ raiseCursorBy 1
  warn $ copyAndNotify_1 n
  liftShelly $ cp (cachePath </> p) (dir </> p)
  copyAndNotify dir ps $ n + 1

-- Acts as a filter for the input to `cleanCache`.
cleanCache :: [T.Text] -> Aura ()
cleanCache [] = cleanCache' 0
cleanCache (input:_)  -- Ignores all but first input element.
  | T.all isDigit input = wrapString (decimal input) >>= (cleanCache' . fst)
  | otherwise         = scoldAndFail $ preCleanCache_1 input

-- Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache' :: Int -> Aura ()
cleanCache' toSave
    | toSave < 0  = scoldAndFail cleanCache_1
    | toSave == 0 = warn cleanCache_2 *> pacman ["-Scc"]  -- Needed?
    | otherwise   = do
        warn $ cleanCache_3 toSave
        okay <- optionalPrompt cleanCache_4
        if not okay
           then scoldAndFail cleanCache_5
           else clean toSave

clean :: Int -> Aura ()
clean toSave = ask >>= \ss -> do
  notify cleanCache_6
  cache <- cacheContents $ cachePathOf ss
  let files     = allFilenames cache
      grouped   = take toSave . reverse <$> groupByName files
      toRemove  = files \\ fold grouped
      filePaths = (cachePathOf ss </>) <$> toRemove
  liftShelly $ traverse_ rm filePaths

-- | Only package files with a version not in any PkgState will be
-- removed.
cleanNotSaved :: Aura ()
cleanNotSaved = asks cachePathOf >>= \path -> do
  notify cleanNotSaved_1
  states <- getStateFiles >>= traverse readState . map fromText
  cache  <- cacheContents path
  let duds = cacheFilter (\p _ -> or (inState p <$> states)) cache
  whenM (optionalPrompt $ cleanNotSaved_2 $ size duds) $
        liftShelly $ traverse_ rm ((path </>) <$> allFilenames duds)

-- Typically takes the contents of the package cache as an argument.
groupByName :: [FilePath] -> [[FilePath]]
groupByName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName (toTextIgnore -> p) = fromMaybe p (Re.span <$> Re.find (Re.regex [] "-[0-9]+") p)

------------
-- REPORTING
------------
reportBadDowngradePkgs :: [T.Text] -> Aura ()
reportBadDowngradePkgs = badReport reportBadDowngradePkgs_1
