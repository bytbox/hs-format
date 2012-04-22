module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Text.Parsec
import Text.Parsec.ByteString

formatString = "$p1 - $p2 [$$$cost]"

data Record = Record
  { p1    :: Int
  , p2    :: Int
  , cost  :: (Int, Int)
  }

fields :: [(String, Record -> String)]
fields =
  [ ("p1", show . p1)
  , ("p2", show . p2)
  , ("cost", (\(d, c) -> show d) . cost)
  ]

sample = Record 3 56 (5, 44)

data FormatPart = Raw Char | Var String
  deriving (Eq, Show)

type Format = [FormatPart]

ident :: Parsec String () FormatPart
ident = do
          char '$'
          x <- many1 (alphaNum <|> char '_')
          return $ Var x

special = do
            char '$' >> char '$'
            return $ Raw '$'

raw :: Parsec String () FormatPart
raw = return . Raw =<< noneOf ['$']

formatParser :: Parsec String () Format
formatParser = do
  xs <- many (try raw <|> try special <|> ident)
  eof
  return xs

renderFormat :: Format -> (String -> a -> String) -> a -> String
renderFormat fmt lu d = concat $ flip map fmt $
  \x -> case x of
          Raw c -> [c]
          Var v -> lu v d

main = do
        case runParser formatParser () "format-string" formatString of
          Left x -> putStrLn $ show x
          Right fmt -> putStrLn $ renderFormat fmt (fromJust . flip lookup fields) sample

