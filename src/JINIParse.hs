module JINIParse where

import Control.Applicative
import Data.Map hiding (empty)

import JParse

-- sample INI file
-- 
-- [section]
-- name = value
-- name2 = "quoted value"
-- [ section2]
-- name = value
-- name2 = "quoted value 2"

type Variables = Map String String
type Sections = Map String Variables
newtype INI = INI Sections deriving Show

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

quotedValue :: Parser String 
quotedValue = bracketed quote (many alphaNumSpace) quote

value :: Parser String 
value = name <|> quotedValue


-- an assignment is a name value pair separated by '=' character
-- we ignore the whitespace around these
assignment :: Parser (String, String)
assignment = do 
    spaces
    name <- name 
    spaces 
    char '='
    spaces 
    value <- value 
    return (name, value)


-- A section has a section header and name-value pairs 
-- separated by '=' character
newline :: Parser Char 
newline = char '\n' <|> char '\r'

newlines :: Parser ()
newlines = many newline >> return ()

blank :: Parser ()
blank = spaces >> newline >> return ()

blanks :: Parser ()
blanks = many blank >> return ()

assignments :: Parser Variables
assignments = fromList <$> many (blanks >> assignment)

section :: Parser (String, Variables)
section = do 
    blanks 
    spaces 
    name <- sectionHeader
    blanks 
    variables <- assignments 
    return (name, variables)


ini :: Parser INI 
ini = INI . fromList <$> many section


sampleText = 
    "[section]" ++ "\n" ++
    "name = value" ++ "\n" ++
    "name2 = \"quoted value\"" ++ "\n" ++
    "[section2]" ++ "\n" ++
    "name = value" ++ "\n" ++
    "name2 = \"quoted value 2\""