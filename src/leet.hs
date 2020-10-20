-- 1047. Remove All Adjacent Duplicates In String
import Data.List.Split
import System.Random
import Control.Monad
import Control.Applicative
import Data.Function
import Data.List

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

sqz [] = [] 
sqz (x:xs) = let ys = sqz xs in case ys of  (y:ys) | x == y -> ys 
                                            _ -> x : ys 

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

data MyRandos = Goose | Verbatim | Vote | GrandSlam | GrandStand | FillErUp | Gander | Volcano | Needlepoint | Gargantuan deriving (Show, Enum, Bounded)

-- instance Random Coin where
--   randomR (a, b) g =
--     case randomR (fromEnum a, fromEnum b) g of
--       (x, g') -> (toEnum x, g')
--   random g = randomR (minBound, maxBound) g

data Coin = Heads | Tails deriving (Show, Enum, Eq, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

main = do
  g <- newStdGen
  print . take 10 $ (randoms g :: [Coin])

  -- [Heads,Tails,Tails,Heads,Tails,Heads,Tails,Heads,Tails,Tails]

process :: [Coin] -> (Int, Int)
process cs = (length cs, length (filter (== Heads) cs))


z1 = zip "abc" "cdef"

testerTwo xs ys = (,) <$> xs <*> ys
testerThree xs ys zs= (,,) <$> xs <*> ys <*> zs

combos :: [[a]] -> [[a]]
combos [] = [[]]
combos ([]:ls) = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

combos_r :: [[a]] -> [[a]]
combos_r [] = [[]]
combos_r ([]:ls) = combos ls
combos_r ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)


filtering xxs = filter (allEqual) xxs

allEqual [] = False
allEqual (x:xs) = all (==x) xs

-- combos [[1,2,3],[3,4,5],[3,6,7]]

solve xs = filter (\x -> length x ==(length xs)) (filtering (combos xs))
solve_ xs = ( map filtering (replicateM (length xs) xs))
-- filter (\x -> length x ==(length xs))
-- solve_a xs = sortBy ( flip compare `on` length) (map ( filter all (\x -> (== ( head (head x) ))))(replicateM (length xs) xs))
solve_a xs = (combos  xs)

-- filter (\x -> all (== (head x)))

-- solve [[1,2,3,9],[3,9,5],[3,6,7,8,9]]
-- solve ["abc","cdea", "ctgza"]

-- ["acc","acc","acz","acz","ac","acc","acc","acz","acz","ac","adc","adc","adz","adz","ad","aec","aec","aez","aez","ae","ac","ac","az","az","a","bcc","bcc","bcz","bcz","bc","bcc","bcc","bcz","bcz","bc","bdc","bdc","bdz","bdz","bd","bec","bec","bez","bez","be","bc","bc","bz","bz","b","ccc","ccc","ccz","ccz","cc","ccc","ccc","ccz","ccz","cc","cdc","cdc","cdz","cdz","cd","cec","cec","cez","cez","ce","cc","cc","cz","cz","c","ccc","ccc","ccz","ccz","cc","ccc","ccc","ccz","ccz","cc","cdc","cdc","cdz","cdz","cd","cec","cec","cez","cez","ce","cc","cc","cz","cz","c","cc","cc","cz","cz","c","cc","cc","cz","cz","c","dc","dc","dz","dz","d","ec","ec","ez","ez","e","c","c","z","z",""]

-- fltr  =  filter all (\x -> (== (head x)))

-- https://wiki.haskell.org/Foldl_as_foldr
foldlByfoldr :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlByfoldr f z0 xs = 
  let leftFold = foldr (\x k -> \z -> k (f z x)) id xs
  in leftFold z0

--   the foldr returns a function that when given an initial accumulator, does a left fold over the list with that accumulator
-- base case: empty list: just return the accumulator (id)
-- inductive step: k is a function that takes an accumulator and does a left fold over the tail of the list, startign with that accumulator
-- and the initial accumulator for the tail needs to be the result of applying f to our initial accumulator and the head of the list
-- so return a function that takes an accumulator z and return k (f z x) (a left fold over the tail of the list with (f z x) as the accumulator
-- really hard to understand this function if you try to manually step through the recursion, but relatively easy this way

  -- https://stackoverflow.com/questions/35084867/haskell-generating-all-combinations-of-n-numbers

twoDigitCombinations = [[x, y] | x <- [0..9], y <- [0..9]]

twoDigitCombinationsM = do
    x <- [0..9]
    y <- [0..9]
    return [x, y]

intermdt_combinationsOfDigits n = foldUpList $ replicate n [0..3]
  where foldUpList [] = return []
        foldUpList (xs : xss) = do
            x <- xs
            ys <- foldUpList xss
            return (x : ys)

combinationsOfDigits n = replicateM n [0..9]
combinationsOfLetters n = replicateM n ['a'..'d']

possibles = sequence $ replicate 2 [0..3]

replicateM_o cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftM2 (:) f (loop (cnt - 1))

-- official remake
replcM_or cnt f =
    loop cnt
  where
    loop cnt
      | cnt <= 0 = pure []
      -- | otherwise =  pure (:) <*> f <*> (loop (cnt -1))
      -- | otherwise =  (:) <$> f <*> (loop (cnt -1))
      | otherwise = liftA2 (:) f (loop (cnt -1))

-- the difference between the above combos and replicateM ->
  -- combos is exhaustive and includes smaller lists
  -- [[1,1],[1,2],[1,3],[1],[2,1],[2,2],[2,3],[2],[3,1],[3,2],[3,3],[3],[1],[2],[3],[]]
  -- replicateM preserves list length
  -- [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
-- herehere
rmvDupes [] = []
rmvDupes (x:xs) = let ys = rmvDupes xs in case ys of
                                          (y:ys) | x == y -> ys
                                          _ -> x:ys 
                                          