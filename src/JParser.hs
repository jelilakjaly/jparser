module JParser where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do
        (a, s') <- x s
        return (f a, s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser sfs <*> Parser sas = Parser $ \s -> do
        (f, s' ) <- sfs s
        (a, s'') <- sas s'
        return (f a, s'')

instance Monad Parser where
    Parser sas >>= f = Parser $ \s -> do
        (a', s') <- sas s
        runParser (f a') s'

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser x <|> Parser y = Parser $ \s -> x s <|> y s


cond :: (Char -> Bool) -> Parser Char
cond f =
    let parsefunc []             = Nothing
        parsefunc (x : xs) | f x = Just (x, xs)
        parsefunc _              = Nothing
    in  Parser parsefunc

isWhiteSpace :: Char -> Bool
isWhiteSpace ' '  = True
isWhiteSpace '\t' = True
isWhiteSpace '\n' = True
isWhiteSpace '\r' = True
isWhiteSpace _    = False


charP :: Char -> Parser Char
charP ch = cond (== ch)

spaceP :: Parser Char
spaceP = cond isWhiteSpace

spacesP :: Parser String
spacesP = many spaceP


stringP :: String -> Parser String
stringP = mapM charP

alphanumP :: Parser Char
alphanumP = cond isAlphaNum

quoteP :: Parser Char 
quoteP = charP '\"'

alphaNumSpaceP :: Parser Char 
alphaNumSpaceP = cond (\c -> isAlphaNum c || isWhiteSpace c)
