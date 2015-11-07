module Srcinfo.Parser where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Megaparsec.Text
import Text.Megaparsec
import Srcinfo.Base
import Control.Monad.Reader

import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt = L.skipLineComment "#"

symbol :: String -> Parser String
symbol = L.symbol sc

arch :: Parser (Maybe String)
arch = (char '_' >> Just <$> many (noneOf " ")) <|> pure Nothing

key :: Parser ParseKey
key =  mkKey <$> many alphaNumChar <*> arch <* sc

value :: Parser String
value = symbol "=" *> many (noneOf "\n") <* symbol "\n"

pair :: Parser Pair
pair = Pair <$> key <*> value

srcinfoFile :: Parser [Pair]
srcinfoFile = sc *> many pair <* eof

parseSrcinfo :: String -> Text -> Either ParseError [Pair]
parseSrcinfo p input = parse srcinfoFile filename input
  where filename = "(" <> p <> ")"

srcinfo :: String -> Text -> Srcinfo
srcinfo s t = case parseSrcinfo s t of
  Left  e   -> error $ show e
  Right src -> toSrcinfo $ reverse src
