import Data.Char
import Data.List

cypher offset val 
  | isSeparator val = val
  | otherwise = chr (((((ord val) - 97) + offset) `mod` 26) + 97)

uncypher offset val 
  | isSeparator val = val
  | otherwise = chr (((((ord val) - 97) - offset) `mod` 26) + 97)

makeSecret msg offset = map (cypher offset) msg

main = do
  putStrLn "enter a message to encode"
  msg <- getLine 
  putStrLn "enter the offset"
  offset <- getLine
  let num = (\x -> read x :: Int) offset
  putStrLn $ makeSecret msg num 

