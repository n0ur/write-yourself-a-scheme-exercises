{-
*Main> parse parseString  "lisp" "\"this \\\"is\\\" a string\""
-}
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

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"")
  char '"'
  return $ String x