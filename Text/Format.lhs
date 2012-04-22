> module Text.Format where

> import Text.Parsec
> import Text.Parsec.ByteString

> data FormatPart = Raw Char | Var String
>   deriving (Eq, Show)

> type Format = [FormatPart]

> parseFormat = runParser formatParser () "format-string"
 
> ident :: Parsec String () FormatPart
> ident = do
>           x <- many1 (alphaNum <|> char '_')
>           return $ Var x

> variable =
>   try (char '$' >> ident)
>   <|>
>   try (char '$' >> between (char '{') (char '}') ident)

> special = do
>             char '$' >> char '$'
>             return $ Raw '$'

> raw :: Parsec String () FormatPart
> raw = return . Raw =<< noneOf ['$']

> formatParser :: Parsec String () Format
> formatParser = do
>   xs <- many (try raw <|> try special <|> variable)
>   eof
>   return xs

> renderFormat :: Format -> (String -> a -> String) -> a -> String
> renderFormat fmt lu d = concat $ flip map fmt $
>   \x -> case x of
>           Raw c -> [c]
>           Var v -> lu v d

