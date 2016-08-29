data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Rational Rational
  | Real Rational
  | Complex (LispVal, LispVal)