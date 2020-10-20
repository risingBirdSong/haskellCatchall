-- 1047. Remove All Adjacent Duplicates In String
import Data.List.Split
import System.Random

-- this is a nice ticket right here
splitted pair lst = concat (splitOn (pair) lst)
-- sTest = splitted "x" "x" "abcxxdef"

removeDupes lst = removeDupes' lst 
removeDupes' []  = []
removeDupes' [x]  = []
removeDupes' (x:y:lst)
  | x /= y = x:removeDupes' (y:lst) 
  | x == y = removeDupes' (concat (splitOn [x,y] (lst))) 

-- concat (splitOn "xx" "abcxxdef")

sec x y lst =  (concat (splitOn [x,y] (lst)))
-- sec 'x' 'x' "abcxxdef" -> "abcdef"

rtrnTypeA :: [a] -> [a]
rtrnTypeA (x:y:ls) = ls

-- removeDupes :: String -> String
-- removeDupes [] = []
-- removeDupes (x:xs) = x : removeDupes (filter (/= x) xs)


rmveD [] = []  
rmveD (x:xs) =  x :  rmveD (filter (/= x) xs)

rmvDu [] = []
rmvDu [x] = [x]
-- rmvDu (x:xs) =  rmvDu (filter (\) xs)

squeeze :: Eq a => [a] -> [a]
squeeze (x:xs) = let ys = squeeze xs in case ys of
                                            (y:ys') | x == y -> ys'
                                            _ -> x:ys
squeeze [] = []

reSqz :: Eq a => [a] -> [a]
-- reSqz [x] = [x]
reSqz [] = []
reSqz (x:xs) = let ys = reSqz xs in case ys of 
                                          (y:ys') | x == y -> ys'
                                          _ -> x:ys

myMain = do
  g <- getStdGen
  print $ take 10 (randomRs ('a', 'z') g)
-- note thisll be the same output per compile

maina = do
  g <- getStdGen
  print $ take 10 (randoms g :: [Double])


maina_rmk = do
    g <- getStdGen
    print $ take 10 (randoms g :: [Int])

-- to get a new sequence each time use newStdGen
-- which will give you a new generator each time it is called:

mainb = do
  g <- newStdGen
  print $ take 5 (randoms g :: [Int])

data Coin = Heads | Tails deriving (Show, Enum, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

coinFlip = do
  g <- newStdGen
  print . take 2 $ (randoms g :: [Coin])