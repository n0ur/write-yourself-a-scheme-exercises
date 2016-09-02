module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio
import Data.Complex

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Float
  | Rational Rational
  | Complex (Complex Double)
  deriving (Show)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> show val -- "Found value"

parseExpr :: Parser LispVal
parseExpr = try parseComplex -- readExpr "3+4i"
  <|> try parseRational -- readExpr "3/4"
  <|> try parseFloat -- readExpr "3.4"
  <|> try parseNumber -- readExpr "3"
  <|> try parseChar -- readExpr "#\\A"
  <|> parseString -- readExpr "\"hello hows it going?\"" | readExpr "\"hello how\\\"s it going?\""
  <|> parseAtom -- readExpr "test"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"")
  char '"'
  return $ String x

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  x <- try (string "space" <|> string "newline")
    <|> many (anyChar)
  return $ Character $ case x of
    "space" -> ' '
    "newline" -> '\n'
    _ -> head x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseExtended

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseExtended :: Parser LispVal
parseExtended = do
  char '#'
  c <- oneOf "xod"
  case c of
    'x' -> liftM (Number . fst . head . readHex) $ many1 (letter <|> digit)
    'o' -> liftM (Number . fst . head . readOct) $ many1 digit
    'd' -> parseDecimal

parseFloat :: Parser LispVal
parseFloat = do
  x <- many digit
  y <- char '.'
  z <- many digit
  let (a, b) = head $ readFloat (x ++ [y] ++ z)
  return (Float a)

parseRational :: Parser LispVal
parseRational = do
  x <- many digit
  char '/'
  z <- many digit
  return $ Rational ((read x) % (read z))

-- "3+4i" gets parsed to: Complex (3.0 :+ 4.0)
parseComplex :: Parser LispVal
parseComplex = do
  x <- (try parseFloat <|> parseDecimal)
  char '+'
  y <- (try parseFloat <|> parseDecimal)
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '\"' -> x
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n