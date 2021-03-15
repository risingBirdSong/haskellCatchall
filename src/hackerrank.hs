import Data.List
import Debug.Trace
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
dropper xss = snd $ mapAccumL (\drp (xs) -> (drp + 1, head (drop drp xs) )) 0 xss

main = do 
  ignore <- getLine 
  a <- prepare 
  b <- prepare
  c <- prepare 
  let firstdiag = sum $ dropper [a,b,c]
  let scnddiag = sum $ dropper $ transpose $ reverse [a,b,c]
  print (abs $ firstdiag - scnddiag)
        where prepare = map (\x -> read x) . words <$> getLine  