data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  deriving (Show)

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  x <- try (string "space" <|> string "newline")
    <|> many (anyChar)
  return $ Character $ case x of
    "space" -> ' '
    "newline" -> '\n'
    _ -> head x

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> try parseNumber
  <|> try parseChar
  <|> parseString