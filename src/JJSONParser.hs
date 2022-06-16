module JJSONParser where

import           Control.Applicative
import           Data.Char
import           JParser

data JValue =
      JNull
    | JBool Bool
    | JNumber Integer
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

intCharP :: Parser Char
intCharP = Parser go
  where
    go (x : xs) | isDigit x = Just (x, xs)
    go _                    = Nothing

intStringP :: Parser String
intStringP = some intCharP

intP :: Parser Integer
intP = read <$> intStringP

jNumberP :: Parser JValue
jNumberP = JNumber <$> intP


stringP :: Parser String
stringP = Parser go
  where
    go []  = Nothing
    go str = Just $ span (/= '"') str

jStringP :: Parser JValue
jStringP = JString <$> (quoteP *> stringP <* quoteP)

--    (*>) :: f a -> f b -> f b
--    a1 *> a2 = (id <$ a1) <*> a2

{-
jStringP :: Parser JValue
jStringP = do
    quoteP
    str <- stringP
    quoteP
    return $ JString str
-}

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
        go [] = Nothing 
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