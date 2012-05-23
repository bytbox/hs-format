module Text.Format
( parseFormat
, renderFormat
, renderFormatString
, scanFormat
, scanFormatString
) where

import Text.Parsec

data FormatParsePart = FRaw Char | FVar String

data FormatPart = Var String (Parsec String () ()) | Raw String

type Format = [FormatPart]

mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

parseFormat :: String -> Either String Format
parseFormat s = 
  case runParser formatParser () "format-string" s of
    Left e -> Left $ show e
    Right x -> cleanFormat x
  where cleanFormat :: [FormatParsePart] -> Either String Format
        cleanFormat ps = mergeRaw "" ps >>= sepVars
        mergeRaw :: String -> [FormatParsePart] -> Either String Format
        mergeRaw s [] = case s of "" -> return []
                                  _ -> return [Raw s]
        mergeRaw s (FRaw c : ps) = mergeRaw (s++[c]) ps
        mergeRaw s (FVar v : ps) = do
                                    r <- mergeRaw "" ps
                                    return $ case s of  "" -> Var v eof : r
                                                        _ -> Raw s : Var v eof : r
        sepVars :: [FormatPart] -> Either String Format
        sepVars [] = return []
        sepVars (Raw s : ps) = do rs <- sepVars ps
                                  return $ Raw s : rs
        sepVars (Var v _ : ps) = case ps of
                                  [] -> return $ [Var v eof]
                                  Raw s : _ -> 
                                    do  rs <- sepVars ps
                                        return $ Var v (ignore $ string s) : rs
                                  Var _ _ : _ -> Left "Ambiguous format string"

ignore = (>>= const (return ()))
ident = many1 (alphaNum <|> char '_') >>= return . FVar
variable = char '$' >> (ident <|> between (char '{') (char '}') ident)
special = string "$$" >> (return $ FRaw '$')
raw = return . FRaw =<< noneOf ['$']

formatParser :: Parsec String () [FormatParsePart]
formatParser = do
  xs <- many (try raw <|> try special <|> variable)
  eof
  return xs

renderFormat :: Format -> (String -> Maybe String) -> Either String String
renderFormat fmt lu = return . concat =<< (sequence $ flip map fmt $
  \x -> case x of
          Raw s -> return s
          Var v _ -> case lu v of
                        Just o -> Right o
                        Nothing -> Left ("No such variable " ++ v))

renderFormatString :: String -> (String -> Maybe String) -> Either String String
renderFormatString s lu = do
                            f <- parseFormat s
                            renderFormat f lu

scanFormat :: Format -> String -> Either String [(String, String)]
scanFormat fmt str = mapLeft show $ runParser scanner () "INPUT" str
  where scanner = do
                    r <- compileScanner fmt
                    eof
                    return r
        compileScanner :: Format -> Parsec String () [(String, String)]
        compileScanner fmt = let ps = map compileScannerPart fmt in
                              foldr (\a as -> do
                                                x <- a
                                                xs <- as
                                                return (x++xs))
                              (return []) ps
        compileScannerPart :: FormatPart -> Parsec String () [(String, String)]
        compileScannerPart p = case p of
                                Raw s -> string s >> return []
                                Var name sep -> do
                                                  d <- manyTill anyChar $ try $ lookAhead sep
                                                  return [(name, d)]

scanFormatString :: String -> String -> Either String [(String, String)]
scanFormatString s str = do
                          f <- parseFormat s
                          scanFormat f str

