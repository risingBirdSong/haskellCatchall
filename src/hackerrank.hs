import Data.List
import Control.Applicative
import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Ratio
import Data.Char
import Data.Ord
exa = "5 6 7 \n3 6 10"

compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets xs ys = foldr (logic) [0,0]  $ zip xs ys
  where logic (bob , alice) [b,a]
          | bob > alice = [1+b,a]
          | bob == alice = [b,a]
          | otherwise = [b,1+a]

mainA = do 
  alice <- map read  . words <$> getLine 
  bob <- map read <$> words <$> getLine  
  mapM_ (\x -> putStr (show x ++ " ") ) $  compareTriplets alice bob

aVeryBigSum :: [Integer] -> Integer
aVeryBigSum ar = sum ar

mainB = do
  ignore <- getLine 
  nums <- map read <$> words <$> getLine 
  print $ aVeryBigSum nums  

exmatrix = [[1,2,3],
            [4,5,6],
            [9,8,9]]

-- dropper :: Traversable t => t [a] -> (Int, t [a])
diags xss = snd $ mapAccumL (\drp (xs) -> (drp + 1, head (drop drp xs) )) 0 xss

mainC = do 
  n <- readLn 
  lines <- parse <$> replicateM n getLine
  let firstdiag = sum $ diags lines
  let scnddiag = sum $ diags $ transpose $ reverse lines
  print (abs $ firstdiag - scnddiag)
  where parse = map (map read . words)



staircase n = zipWith (++) blanks stairs
  where stairs = take n $ iterate (++"#") "#"
        blanks = reverse $ take n $ iterate (++" ") ""

mainD = do 
  tonum <- readLn :: IO Int 
  let makestairs = staircase tonum
  mapM_ putStrLn makestairs


-- Mini-Max Sum
-- nums :: [Integer]
nums = [1, 2, 3, 4, 5]



-- miniMaxSum xs = let [a,b] = sum <$> ([init, tail] <*> [xs]) in [a,b]
-- miniMaxSum xs = sum <$> ([init, tail] <*> [xs])
miniMaxSum xs = sum <$> ([init, tail] <*> [sort xs])
 
mainE = do 
  rawxs <- getLine 
  let xs = map read $ words rawxs
  let [a,b] = miniMaxSum xs 
  putStrLn (show a ++ " " ++ show b)


birthdayCakeCandles xs = length $ maximumBy (comparing length) $ group $ sort xs 

mainF = do 
  ignore <- getLine 
  rawlist <- getLine 
  let list = (map read $ words rawlist) :: [Integer] 
  print $ birthdayCakeCandles list

  
-- Time Conversion
