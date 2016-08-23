module Main where
import System.Environment

main :: IO ()
main = do
  (firstName:lastName:_) <- getArgs
  putStrLn $ "Hello " ++ firstName ++ " " ++ lastName