module Main where

import Text.Format

formatString = "$p1 - ${p2} [$$$cost]"

data Record = Record
  { p1    :: Int
  , p2    :: Int
  , cost  :: (Int, Int)
  }

fields :: String -> Maybe (Record -> String)
fields "p1" = return $ show . p1
fields "p2" = return $ show . p2
fields "cost" = return $ (\(d, _) -> show d) . cost
fields _ = Nothing

postponeMaybe :: (a -> Maybe (b -> c)) -> a -> b -> Maybe c
postponeMaybe f a b = do
                        f' <- f a
                        return $ f' b

ff :: Record -> String -> Maybe String
ff = flip $ postponeMaybe fields

sample = Record 3 56 (5, 44)
sampleString = "3 - 56 [$5]"

scanMain = scanFormatString formatString sampleString

renderMain = renderFormatString formatString (ff sample)

main = case scanMain of
        Left s -> putStrLn s
        Right s -> print s

