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

{-|
  Parse a format string into a `Format` object ready to be used in
  `renderFormat` and `scanFormat`.
 
  The format string consists of raw tokens (ordinary characters), and
  variables, marked '$varname' or '${varname}'. The dollar sign may be used as
  a raw token by escaping it with another dollar sign, like so: '$$'. (If you
  want a variable named $, use '${$}'.)

  Not all syntactically valid parse strings are semantically valid. In
  particular, two variables must not occur consecutively without interleaving
  raw tokens. (If this were permitted, the resulting grammar would be
  ambiguous.)

  Variable names may be used twice; however, this will make the result of
  `scanFormat` somewhat difficult to deal with.
  
  The functions `renderFormatString` and `scanFormatString` are provided as
  conveniences to make doing this explicitly unnecessary.
 -}
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

-- |A more convenient alternative to using `parseFormat` and `renderFormat`.
renderFormatString :: String -> (String -> Maybe String) -> Either String String
renderFormatString s lu = do
                            f <- parseFormat s
                            renderFormat f lu

{-|
  Parses a string using the given format as a guide, generating a list of pairs
  of variable names and values.
  
  To determine where a variable ends, the entire subsequent string of raw
  tokens (until the next variable or the end of the string) is used as a
  terminator. It must occur verbatim in the scanned string or the parse will
  fail. The smallest match is used: if the format string is '${a}:' and the
  input string is '1:2:', the parse will exit with an error, as only the first
  character will be considered part of the variable
  'a'.
 -}
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

-- |A more convenient alternative to using `parseFormat` and `scanFormat`.
scanFormatString :: String -> String -> Either String [(String, String)]
scanFormatString s str = do
                          f <- parseFormat s
                          scanFormat f str

