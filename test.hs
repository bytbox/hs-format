module Main where

import System.Exit (exitSuccess, exitFailure)
import Test.QuickCheck
import Text.Format

prop_render_raw_idempotent :: String -> Bool
prop_render_raw_idempotent s =
  let r = renderFormatString "$a" (\_ -> Just s) in
  r == Right s
prop_scan_raw_idempotent s =
  let r = scanFormatString "$a" s in
  r == Right [("a", s)]

tests = [ ("RenderRawIdempotent", prop_render_raw_idempotent)
        , ("ScanRawIdempotent", prop_scan_raw_idempotent)
        ]

isSuccess (Success _ _ _) = True
isSuccess _ = False

main = do
        r <- mapM (\(n, p) -> putStr n >> quickCheckResult p) tests
        if and $ map isSuccess r
          then exitSuccess
          else exitFailure

