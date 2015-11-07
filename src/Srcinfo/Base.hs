module Srcinfo.Base where

import qualified Data.Map.Lazy as M
import Data.Monoid ((<>))

data ParseKey = Key String (Maybe String)
              | Pkgbase
              | Pkgname
              deriving (Show)

type Key = (String, Maybe String)

mkKey :: String -> Maybe String -> ParseKey
mkKey ("pkgbase") (Nothing) = Pkgbase
mkKey ("pkgbase") (Just _) = error "pkgbase cannot have an archetecture"
mkKey ("pkgname") (Nothing) = Pkgname
mkKey ("pkgname") (Just _) = error "pkgname cannot have an archetecture"
mkKey key arch = Key key arch

data Pair = Pair ParseKey String
          deriving (Show)

type Section = M.Map Key [String]
data Srcinfo = Srcinfo Section [(String, Section)]
             deriving (Show)

type Script = [Pair]

-- I realy want a general purpose version of this, but it seems restricted
-- to Maybe
(<+>) :: Monoid a => Maybe a -> Maybe a -> Maybe a
Nothing <+> Nothing = Nothing
Nothing <+> Just b = Just b
Just a <+> Nothing = Just a
Just a <+> Just b = Just $ a <> b

getVar :: Srcinfo -> String -> Key -> Maybe [String]
getVar (Srcinfo sec secs) pkg k = getFromSecList secs pkg k <+> getVar' sec k 

getFromSecList :: [(String, Section)] -> String -> Key -> Maybe [String]
getFromSecList [] _ _ = Nothing
getFromSecList _ "" _ = Nothing
getFromSecList ((name, sec) : secs) pkg k = case name == pkg of
  True -> getVar' sec k
  False -> getFromSecList secs pkg k

getVar' :: Section -> Key -> Maybe [String]
getVar' sec k@(_, Nothing) = M.lookup k sec
getVar' sec k@(s, (Just _)) = getVar' sec (s, Nothing) <+> M.lookup k sec 

toSrcinfo :: Script -> Srcinfo
toSrcinfo [] = Srcinfo M.empty []
toSrcinfo (Pair Pkgbase _ : rs) = toSrcinfo rs
toSrcinfo (Pair Pkgname name : rs) = newPkgName name $ toSrcinfo rs
toSrcinfo (Pair (Key name arch) value : rs) = insert (name, arch) value $ toSrcinfo rs

newPkgName :: String -> Srcinfo -> Srcinfo
newPkgName name (Srcinfo base pkgs) = Srcinfo base $ (name, M.empty): pkgs

insert :: Key -> String -> Srcinfo -> Srcinfo
insert key value (Srcinfo base pkgs) = case null pkgs of
  True -> Srcinfo (union key value base) pkgs
  False -> Srcinfo base ((fst $ head pkgs , union key value $ snd $ head pkgs) : tail pkgs)

union :: Key -> String -> Section -> Section
union key value section = case M.lookup key section of
  Nothing -> M.insert key [value] section
  Just vs -> M.insert key (value : vs) section

