module Main where

import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

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
  | Vector (Array Int LispVal)
instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String [LispVal]
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "an error has occured!!"
  strMsg = Default

type ThrowsError = Either LispError

main :: IO ()
main = do
  args   <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Character _)          = return val
eval val@(Number _)             = return val
eval val@(Float _)              = return val
eval val@(Rational _)           = return val
eval val@(Complex _)            = return val
eval val@(Bool _)               = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args))    = mapM eval args >>= apply func 
eval val@(Atom _)               = return val
eval val@(List _)               = return val
eval val@(DottedList _ _)       = return val
eval val@(Vector _)             = return val
-- eval badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func) 
                        ($ args)
                        (lookup func primitives)
  -- case lookup func primitives of
  -- Just f -> f args
  -- Nothing -> Bool False

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "expected " ++ show expected ++ " args. found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid types. expected " ++ show expected ++ " found values " ++ show found
showError (Parser error)                = "Parser error at " ++ show error

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showVal :: LispVal -> String
showVal (String s)       = "\"" ++ s ++ "\""
showVal (Character s)    = "'" ++ [s] ++ "'"
showVal (Atom s)         = s
showVal (Number s)       = show s
showVal (Float s)        = show s
showVal (Rational s)     = show s
showVal (Complex s)      = show s
showVal (Bool True)      = "#t"
showVal (Bool False)     = "#f"
showVal (List l)         = "(" ++ unwordsList l ++ ")"
showVal (DottedList l c) = "(" ++ unwordsList l ++ " . " ++ showVal c ++ ")"
showVal (Vector l)       = "#(" ++ unwordsList (elems l) ++ ")"

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("string?", isString),
              ("symbol?", isSymbol),
              ("number?", isNumber)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op []       = throwError $ NumArgs 2 []
numericBinOp func params = mapM unpackNum params >>= return . Number . foldl1 func

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" [notNum]

parseExpr :: Parser LispVal
parseExpr = try parseComplex -- readExpr "3+4i"
  <|> try parseRational -- readExpr "3/4"
  <|> try parseFloat -- readExpr "3.4"
  <|> try parseNumber -- readExpr "3"
  <|> try parseChar -- readExpr "#\\A"
  <|> try parseString -- readExpr "\"hello hows it going?\"" | readExpr "\"hello how\\\"s it going?\""
  <|> try parseAtom -- readExpr "test"
  <|> try parseBool -- readExpr "#t"
  <|> try (do -- readExpr "#(1 2 3)"
      string "#("
      x <- parseVector
      char ')'
      return x)
  <|> do
    char '('
    x <- (try parseList <|> parseDottedList)
    char ')'
    return x
  <|> parseQuote -- readExpr "'(1 2)"
  <|> parseQuasiQuote -- readExpr "`(1 2)"
  <|> parseUnquote -- readExpr ",(1 2)"

-- a list is: (3.2 4 5/4 3+4i) => List [Float 3.2,Number 4,Rational (5 % 4),Complex (3.0 :+ 4.0)]
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces 

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail  <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

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
  return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  b <- char 't' <|> char 'f'
  return $ case b of
    't' -> Bool True
    'f' -> Bool False

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

parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasi quote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

isString :: [LispVal] -> ThrowsError LispVal
isString (String _:_) = return $ Bool True
isString _            = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol (Atom _:_) = return $ Bool True
isSymbol _          = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber (Number _:_) = return $ Bool True
isNumber _            = return $ Bool False