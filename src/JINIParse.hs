module JINIParse where

import Control.Applicative

import JParse

-- sample INI file
-- 
-- [section]
-- name = value
-- name2 = "quoted value"
-- [ section2]
-- name = value
-- name2 = "quoted value 2"

bracketed :: Parser a -> Parser b -> Parser c -> Parser b
bracketed pa pb pc = do 
    pa 
    b <- pb 
    pc 
    return b

bracketOpen :: Parser Char 
bracketOpen = char '['

bracketClose :: Parser Char 
bracketClose = char ']'


sectionName :: Parser String 
sectionName = bracketed spaces (some alphanum) spaces

sectionHeader :: Parser String 
sectionHeader = bracketed bracketOpen sectionName bracketClose

name :: Parser String 
name = some alphanum

quote :: Parser Char 
quote = char '\"'