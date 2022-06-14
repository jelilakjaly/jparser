module JINIParser where

import Control.Applicative
import Data.Map hiding (empty)

import JParser

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
bracketOpen = charP '['

bracketClose :: Parser Char 
bracketClose = charP ']'


sectionName :: Parser String 
sectionName = bracketed spacesP (some alphanumP) spacesP

sectionHeader :: Parser String 
sectionHeader = bracketed bracketOpen sectionName bracketClose

name :: Parser String 
name = some alphanumP

quotedValue :: Parser String 
quotedValue = bracketed quoteP (many alphaNumSpaceP) quoteP

value :: Parser String 
value = name <|> quotedValue


-- an assignment is a name value pair separated by '=' character
-- we ignore the whitespace around these
assignment :: Parser (String, String)
assignment = do 
    spacesP
    name <- name 
    spacesP
    charP '='
    spacesP 
    value <- value 
    return (name, value)


-- A section has a section header and name-value pairs 
-- separated by '=' character
newline :: Parser Char 
newline = charP '\n' <|> charP '\r'

newlines :: Parser ()
newlines = many newline >> return ()

blank :: Parser ()
blank = spacesP >> newline >> return ()

blanks :: Parser ()
blanks = many blank >> return ()

assignments :: Parser Variables
assignments = fromList <$> many (blanks >> assignment)

section :: Parser (String, Variables)
section = do 
    blanks 
    spacesP 
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