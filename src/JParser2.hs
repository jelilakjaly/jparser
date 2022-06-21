{-# LANGUAGE FlexibleInstances #-}
module JParser2 where

import           Control.Applicative
import           Data.Map                       ( Map )
import qualified Data.Map                      as M

data Input = Input
    { inputLine :: Integer
    , inputStr  :: String
    }
    deriving (Show, Eq)

newtype Parser a = Parser {runParser :: Input -> Either Integer (a, Input)}

instance Functor Parser where
    fmap f (Parser x) = Parser $ \input -> do
        (a, input') <- x input
        return (f a, input')

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    Parser inf <*> Parser inx = Parser $ \input -> do
        (f, input' ) <- inf input
        (x, input'') <- inx input'
        return (f x, input'')

instance Monad Parser where
    Parser x >>= f = Parser $ \input -> do
        (a, input') <- x input
        runParser (f a) input'

instance Alternative (Either Integer) where
    empty = Left 0
    Left _ <|> x = x
    x      <|> _ = x

instance Alternative Parser where
    empty = Parser $ \input -> Left 0
    Parser x <|> Parser y = Parser $ \input -> x input <|> y input



cond :: (Char -> Bool) -> Parser Char
cond f = Parser parse
  where
    parse (Input n (x : xs)) | f x && x == '\n' = Right (x, Input (n + 1) xs)
    parse (Input n (x : xs)) | f x              = Right (x, Input n xs)
    parse (Input n _)                           = Left n

mkCharP :: Char -> Parser Char
mkCharP c = cond (== c)

mkStringP :: String -> Parser String
mkStringP = mapM mkCharP

ws :: Parser Char
ws = mkCharP ' ' <|> mkCharP '\n' <|> mkCharP '\t' <|> mkCharP '\r'

wss :: Parser String
wss = many ws


normalCharP :: Parser Char
normalCharP = cond f
  where
    f :: Char -> Bool
    f c = c `notElem` ['\"', ' ', '\n', '\r', '\t', ']', '[', '=']

headerP :: Parser String
headerP = do
    mkCharP '['
    wss
    val <- some normalCharP
    wss
    mkCharP ']'
    return val

-- value could be a normal string or string wrapped in quotes
valP :: Parser String
valP = (mkCharP '\"' *> some (normalCharP <|> mkCharP ' ') <* mkCharP '\"')
    <|> some (normalCharP <|> mkCharP ' ')

keyValP :: Parser (String, String)
keyValP = do
    wss
    key <- some normalCharP
    wss
    mkCharP '='
    wss
    val <- valP
    wss
    return (key, val)

sectionP :: Parser (Map String (Map String String))
sectionP = do
    header <- headerP
    wss
    vals <- some keyValP
    wss
    let values = M.fromList vals
    return $ M.insert header values M.empty

iniP :: Parser [Map String (Map String String)]
iniP = do
    wss
    vals <- some sectionP
    wss
    return vals


sample =
    "[section]"
        ++ "\n"
        ++ "name = value"
        ++ "\n"
        ++ "name2 = \"quoted value\""
        ++ "\n"
        ++ "[ section2]"
        ++ "\n"
        ++ "name = value"
        ++ "\n"
        ++ "name2 = \"quoted value 2\""






