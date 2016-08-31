import Data.Ratio
import Data.Complex

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Rational Rational
  | Real Double -- like Float type in ch2_6
  | Complex (Complex Double)
  deriving (Show)

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

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> try parseComplex
  <|> try parseRational
  <|> parseNumber
  <|> parseString