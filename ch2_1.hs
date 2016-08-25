parseNumber :: Parser LispVal
parseNumber = do
  r <- many1 digit
  let n = Number $ read r
  return n

parseNumber2 :: Parser LispVal
parseNumber2 = many1 digit >>= (\x -> return $ Number (read x))