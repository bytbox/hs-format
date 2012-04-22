module Main where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Text.Format

formatString = "$p1 - ${p2} [$$$cost]"

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

main = case parseFormat formatString of
        Left x -> putStrLn $ show x
        Right fmt -> putStrLn $ renderFormat fmt (fromJust . flip lookup fields) sample

