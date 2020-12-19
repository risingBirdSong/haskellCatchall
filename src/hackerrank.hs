import Data.List

alice = [1,2,3]
bob = [3,2,1]
tripletCompare (x,y) (a,b)
  | x > y = (succ a, b)
  | x < y = (a, succ b)
  | otherwise = (a,b)

compareTriplets a b = pairToList $ foldr tripletCompare (0,0) $ zip a b 
pairToList (a,b) = [a,b]

main :: IO()
main = do 
  aTemp <- getLine 
  let a = map (read :: String -> Int) $ words aTemp
  bTemp <- getLine  
  let b = map (read :: String -> Int) $ words bTemp
  let ans = compareTriplets a b
  print ans 
  let result = intercalate " " $ map show ans
  putStrLn result
  return ()