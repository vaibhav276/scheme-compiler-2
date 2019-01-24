module Parser where

-- import           Control.Monad
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
               | List [LispVal]
               | DottedList [LispVal] LispVal
               | Number Integer
               | String String
               | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

-- | Returns a list of errors or Nothing if everything parses
readExpr :: String -> Maybe [String]
readExpr input = case parse parseExpr "lisp" input of
  Left err -> Just $ lines ( show err )
  Right _  -> Nothing

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- ( try parseList ) <|> parseDottedList
                   char ')'
                   return x

{- Individual element parsers -}

-- | parses strings
parseString :: Parser LispVal
parseString = do char '"'
                 x <- many ( noneOf "\"" )
                 char '"'
                 return $ String x

-- | parses atoms
-- which is a letter or symbol, followed by any number of letters, digits or symbols
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many ( letter <|> digit <|> symbol )
               let atom = [first] ++ rest
               return $ case atom of
                 "#t"      -> Bool True
                 "#f"      -> Bool False
                 otherwise -> Atom atom

-- | parses numbers
parseNumber :: Parser LispVal
-- parseNumber = fmap ( Number . read ) $ many1 digit
parseNumber = do nStr <- many1 digit
                 return $ ( Number . read ) nStr

-- | parses lists
parseList :: Parser LispVal
-- parseList = liftM List $ sepBy parseExpr spaces
parseList = do vals <- sepBy parseExpr spaces
               return $ List vals

-- | parses dotted lists
parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

-- | parses quoted lists
parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]
