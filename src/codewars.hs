import Data.List
import Data.Bool
import Data.Function


automorphic_a :: Integer -> String
automorphic_a n = bool "Not!!" "Automorphic" $ show n `isSuffixOf` show (n^2)

sortNumbers :: [Int] -> Maybe [Int]
sortNumbers [] = Nothing
sortNumbers xs =  Just (sort xs)

automorphic :: Integer -> String
automorphic num 
  | amph num = "Automorphic"
  | otherwise = "Not!!"
amph num = num == end
          where end = digitsToEnd num

getend digits n = toNum (drop ((length digits) - (length (convertBtr n))) digits) 
sqrThenLst n = convertBtr (n*n) 

digitsToEnd num = getend (sqrThenLst num) num

convertSlow 0 = []
convertSlow num =  convertSlow (num `div` 10) ++ [snd (num `divMod` 10)]

convertBtr num = reverse (convertBtr' num)
convertBtr' 0 = []
convertBtr' num = snd (num `divMod` 10) : convertBtr' (num `div` 10)

toNum lst = fst $ foldr (\x (acc,place) -> (acc + (place * x) , place * 10) ) (0,1) lst

sqr n = n * n 

strangeAdd a b = length (concat [ones a , ones b])
              where ones n = take n $ repeat "this is a weird way to add" 

-- ones n = take n $ repeat 1 
-- Minimize Sum Of Array (Array Series #1)
-- minSum :: Num b => [b] -> [b]
minSum :: (Num a, Ord a) => [a] -> a
minSum nums = sum $ take half 
                  $ zipWith (*) (srted) (reverse srted)
                  where srted = sort nums
                        half = (length nums `div` 2)


aa ls = (zip <*> reverse) $ sort ls 

maxNumber :: Int -> Int 
maxNumber n = read $ reverse $ sort $ show n 

paddedNums n = "Value is " ++ (reverse $ take 5 $ reverse $ ("0000" ++ show n))

getSum :: (Num a, Ord a, Enum a) => a -> a -> a
getSum a b = sum [(min a b)..(max a b)]


-- cnsPrs n = not . null $ filter (\(x,y) -> x + y == n) $ zip [1..n] (tail [1..n])
consecutiveDucks n = n `elem` zipWith (+) rng (tail rng) 
                  where rng = [1..n]

cd n = filter (==n) $ zipWith (+) rng (tail rng) 
                  where rng = [1..n+1]

manyanswers n = map cd [1..n]                  

-- hmm why doesn't this work?
