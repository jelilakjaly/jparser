module JParse where

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
    Parser x <|> Parser y = Parser $ \s -> case x s of
        Nothing -> y s
        Just v  -> Just v


conditional :: (Char -> Bool) -> Parser Char
conditional f =
    let parsefunc []             = Nothing
        parsefunc (x : xs) | f x = Just (x, xs)
        parsefunc _              = Nothing
    in  Parser parsefunc


char :: Char -> Parser Char
char ch = conditional (== ch)

isWhiteSpace :: Char -> Bool
isWhiteSpace ' '  = True
isWhiteSpace '\t' = True
isWhiteSpace '\n' = True
isWhiteSpace '\r' = True
isWhiteSpace _    = False

space :: Parser Char
space = conditional isWhiteSpace

spaces :: Parser String
spaces = many space


string :: String -> Parser String
string = mapM char

alphanum :: Parser Char
alphanum = conditional isAlphaNum
