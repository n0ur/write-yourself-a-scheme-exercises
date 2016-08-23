module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let nums = map read args
  putStrLn $ "Sum of numbers " ++ show (sum nums)