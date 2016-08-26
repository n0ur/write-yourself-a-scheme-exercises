import Numeric

{-
*Main Prelude> readExpr "#xe"
"Number 14"
*Main Prelude> readExpr "#o077"
"Number 63"
*Main Prelude> readExpr "#x3F"
"Number 63"
*Main Prelude> readExpr "63"
"Number 63"
-}

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

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseExtended

parseExpr :: Parser LispVal
parseExpr = parseNumber
  <|> parseString
  <|> parseAtom