module JJSONParser where

import Control.Applicative
import Data.Char 
import JParser

data JValue =
      JNull
    | JBool Bool
    | JNumber Integer
    | JString String
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Show, Eq)

jNullP :: Parser JValue
jNullP = JNull <$ stringP "null"

jBoolP :: Parser JValue 
jBoolP = JBool True <$ stringP "true" <|> JBool False <$ stringP "false"

intCharP :: Parser Char 
intCharP = Parser go 
    where 
        go (x:xs) | isDigit x = Just (x, xs)
        go _  = Nothing

intStringP :: Parser String 
intStringP = some intCharP

intP :: Parser Integer 
intP = read <$> intStringP

jNumberP :: Parser JValue 
jNumberP = JNumber <$> intP