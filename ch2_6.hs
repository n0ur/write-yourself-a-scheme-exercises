data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Float Float
  deriving (Show)

parseFloat :: Parser LispVal
parseFloat = do
  x <- many digit
  y <- char '.'
  z <- many digit
  let (a, b) = head $ readFloat (x ++ [y] ++ z)
  return (Float a)

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> try parseFloat -- notice try here
  <|> parseNumber
  <|> parseString