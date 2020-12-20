import Data.List

alice = [1,2,3]
bob = [3,2,1]
tripletCompare (x,y) (a,b)
  | x > y = (succ a, b)
  | x < y = (a, succ b)
  | otherwise = (a,b)

compareTriplets a b = pairToList $ foldr tripletCompare (0,0) $ zip a b 
pairToList (a,b) = [a,b]

maina :: IO()
maina = do 
  aTemp <- getLine 
  let a = map (read :: String -> Int) $ words aTemp
  bTemp <- getLine  
  let b = map (read :: String -> Int) $ words bTemp
  let ans = compareTriplets a b
  print ans 
  let result = intercalate " " $ map show ans
  putStrLn result
  return ()

hello_world = putStrLn "hello world"

-- f :: Int -> [Int] -> [Int]
fb n arr = concatMap (replicate  n ) arr

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> fb n arr). map read. words

myFilter pred [] = [] 
myFilter pred (l:ls) 
  | pred l = l : myFilter pred ls
  | otherwise  = myFilter pred ls