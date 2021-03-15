import Data.List
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