module JJSONParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           JParser
import           Numeric

data JValue =
      JNull
    | JBool Bool
    | JNumber Double
    | JString String
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Show, Eq)

jValueP :: Parser JValue
jValueP = jNullP <|> jBoolP <|> jNumberP <|> jStringP <|> jArrayP <|> jObjectP

jNullP :: Parser JValue
jNullP = JNull <$ mkStringP "null"

jBoolP :: Parser JValue
jBoolP = JBool True <$ mkStringP "true" <|> JBool False <$ mkStringP "false"

{-
intCharP :: Parser Char
intCharP = Parser go
  where
    go (x : xs) | isDigit x = Just (x, xs)
    go _                    = Nothing

intStringP :: Parser String
intStringP = some intCharP

intP :: Parser Integer
intP = read <$> intStringP
-}

doubleFromParts :: Integer -- sign
                -> Integer -- integral part
                -> Double  -- decimal part
                -> Integer -- exponent
                -> Double
doubleFromParts sign int dec expo = 
    fromIntegral sign * (fromInteger int + dec) * (10 ^^ expo)

doubleLiteral :: Parser Double 
doubleLiteral = doubleFromParts
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> ((read <$> (('0':) <$> ((:) <$> mkCharP '.' <*> digits))) <|> pure 0)
    <*> ((e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits))) <|> pure 0)
    where 
        digits = some $ parseIf isDigit
        minus = (-1) <$ mkCharP '-'
        plus = 1 <$ mkCharP '+'
        e = mkCharP 'e' <|> mkCharP 'E'

jNumberP :: Parser JValue
jNumberP = JNumber <$> doubleLiteral


parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser go
  where
    go :: String -> Maybe (Char, String)
    go (x : xs) | f x = Just (x, xs)
    go _              = Nothing

escapeUnicodeP :: Parser Char
escapeUnicodeP =
    chr . fst . head . readHex <$> replicateM 4 (parseIf isHexDigit)

escapeCharP :: Parser Char
escapeCharP =
    ('"' <$ mkStringP "\\\"")
        <|> ('\\' <$ mkStringP "\\\\")
        <|> ('/' <$ mkStringP "\\/")
        <|> ('\b' <$ mkStringP "\\b")
        <|> ('\f' <$ mkStringP "\\f")
        <|> ('\n' <$ mkStringP "\\n")
        <|> ('\r' <$ mkStringP "\\r")
        <|> ('\t' <$ mkStringP "\\t")
        <|> (mkStringP "\\u" *> escapeUnicodeP)

normalCharP :: Parser Char
normalCharP = parseIf ((&&) <$> (/= '"') <*> (/= '\\'))

stringLiteralP :: Parser String
stringLiteralP = quoteP *> many (normalCharP <|> escapeCharP) <* quoteP

jStringP :: Parser JValue
jStringP = JString <$> stringLiteralP 

bracketOpenP :: Parser Char
bracketOpenP = mkCharP '['

bracketCloseP :: Parser Char
bracketCloseP = mkCharP ']'

commaP :: Parser Char
commaP = mkCharP ','

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jArrayP :: Parser JValue
jArrayP = do
    bracketOpenP
    spacesP
    vals <- sepBy (spacesP *> commaP <* spacesP) jValueP
    spacesP
    bracketCloseP
    return $ JArray vals


braceOpenP :: Parser Char
braceOpenP = mkCharP '{'

braceCloseP :: Parser Char
braceCloseP = mkCharP '}'

colonP :: Parser Char
colonP = mkCharP ':'

keyP :: Parser String
keyP = Parser go
  where
    go []  = Nothing
    go str = Just (span (/= ':') str)

objectP :: Parser (String, JValue)
objectP = do
    key <- keyP
    spacesP
    mkCharP ':'
    spacesP
    val <- jValueP
    spacesP
    return (key, val)


jObjectP :: Parser JValue
jObjectP = do
    braceOpenP
    spacesP
    vals <- sepBy (spacesP *> commaP <* spacesP) objectP
    spacesP
    braceCloseP
    return $ JObject vals


parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filePath parser = do
    input <- readFile filePath
    return (fst <$> runParser parser input)
