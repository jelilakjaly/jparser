module JParse where

import           Control.Applicative

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


char :: Char -> Parser Char
char ch = Parser charP
  where
    charP :: String -> Maybe (Char, String)
    charP []       = Nothing
    charP (x : xs) = if x == ch then Just (x, xs) else Nothing

space :: Parser Char
space = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

spaces :: Parser String 
spaces = many space
