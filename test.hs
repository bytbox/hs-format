module Main where

import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck
import Text.Format

prop_id :: Int -> Bool
prop_id = const True

prop_id' :: Int -> Bool
prop_id' = const False

tests = [ ("Good", prop_id)
        , ("Bad", prop_id')
        ]

isSuccess (Success _ _ _) = True
isSuccess _ = False

main = do
        r <- mapM (\(n, p) -> putStr n >> quickCheckResult p) tests
        if and $ map isSuccess r
          then exitSuccess
          else exitFailure

