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

fc :: Int -> [Int] -> [Int]
fc n [] = [] 
fc n (a:arr) 
  | a < n = a : fc n arr
  | otherwise = fc n arr

f :: [Int] -> [Int]
f lst = map snd $ filter (\(i,v) -> even i ) $ zip [0..] lst

fn n = map (\x -> fromIntegral x :: Int) [0..n-1]


century n = (year `div` 100 ) + 1
  where year = n - 1


safeIndex idx lst = go zipped 
  where zipped = zip [0..] lst
        go [] = Nothing 
        go  ((i, val):ls)
            | idx == i = (Just val) 
            | otherwise = go ls    
matrixA = [
  [1,2,3],
  [4,5,6],
  [7,8,9]
  ] 

-- safeIndex idx lst = ( safeIndex idx (safeIndex (idx) lst) ) : go (succ idx) idx

-- diagonalDiff_primary lst = go 0 
--   where go idx lst
