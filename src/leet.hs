-- 1047. Remove All Adjacent Duplicates In String
import Data.List.Split
import System.Random
import Control.Monad
import Control.Applicative
import Data.Function
import Data.List
import Test.QuickCheck
import qualified Data.Text as T
import Data.Bool
import Debug.Trace
 
-- this one worked!
-- stack ghc --package QuickCheck -- MyProgram.hs
-- https://stackoverflow.com/questions/53402263/could-not-find-module-test-quickcheck-on-windows

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

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
                                          
rmvDupes_a [] = []
rmvDupes_a (x:xs) = let ys = rmvDupes_a xs in case ys of
                                              (y:ys) | x == y -> ys
                                              _ -> x:ys

rmvDupes_b [] = []
rmvDupes_b (x:xs) = let ys = rmvDupes_b xs in case ys of
  (y:ys) | x == y -> ys
  _ -> x:ys

  -- [5,7,3,9,4,9,8,3,1]
grpSrt xs = (group (sort xs))
-- [[1],[3,3],[4],[5],[7],[8],[9,9]]
fltrLength  xs = (filter ((==1) . length )) xs -- [[1],[3,3],[4],[5],[7],[8],[9,9]]
-- [[1],[4],[5],[7],[8]]
lrgUniqNum_testa xs =  sortBy ( compare `on` length) (group (sort xs))
lrgUniqNum xs = head $ last $ sortBy (flip compare `on` length) (group (sort xs))
lrgUniqNumA xs = head $ head $ reverse $ (filter ((==1) . length )) (group (sort xs))
lrgUniqNumB xs = head $ concat $ reverse $ (filter ((==1) . length )) (group (sort xs))

-- lrgUniqNum xs = filter ((==1).length) ( group (sort xs))

-- https://stackoverflow.com/questions/21410293/filter-list-items-by-length-in-haskell
-- ah i was getting the orering wrong, in flterTest remember length is a func so it gets first to the incoming args 
flterTest xs = filter ((==1).length) xs 
flterTesta xs = filter  (\x -> length x == 1)  xs 


removeDup :: Eq a => [a] -> [a]
removeDup = go False
  where
    go _ [] = []
    go True (x : y : xs) = go (x == y) (y : xs)
    go True [x] = []
    go False (x : y : xs)
      | x == y = go True (y : xs)
      | otherwise = x : go False (y : xs)
    go False [x] = [x]

lrgstUniqNum :: (Eq a, Ord a) => [a] -> a
lrgstUniqNum = head . removeDup . sortBy (flip compare)
lrgstUniqNumTest  :: (Eq a, Ord a) => [a] -> [a]
lrgstUniqNumTest = removeDup . sortBy (flip compare) 

-- https://www.codewars.com/kata/523a86aa4230ebb5420001e1/train/haskell

anag x y = x' == y'
  where x' = sort x
        y' = sort y

keepAnas str xs = filter (anag str) xs

-- https://leetcode.com/problems/running-sum-of-1d-array/
-- -- runningSum :: [a] -> a -> a
runningSum lst = runningSum' lst 0
runningSum' [] sm = [] 
runningSum' (a:xs) sm = (a + sm) : (runningSum' (xs) (a + sm))

test []  = []
test (x:xs) = (x+1) : test xs 

rnningsm lst = tail $ foldl (\acc x ->  acc ++ [ x + (last acc)]) [0] lst


-- dosomething x y = (x * y :: _) + 2

-- 1431. Kids With the Greatest Number of Candies

findMax lst = last $ sort lst
grtsc lst x = map (\v -> if v >= x then True else False ) lst 
solvecands lst extra = grtsc dist originalMax
    where originalMax = findMax lst
          dist = map (+extra) lst


-- 1470. Shuffle the Array
firstHalf lst = take (length lst `div` 2) lst
secondHalf lst = drop (length lst `div` 2) lst

zipEm front back = zip front back

tupleToList lst = foldr (\(f,s) a -> f : s : a) [] lst


joinEm :: Foldable t => t (a, a) -> [a]
joinEm lst = foldl (\acc x -> acc ++ [fst x, snd x] ) [] lst
-- shuffleArr :: [Int] -> [Int]
-- shuffleArr lst = 

candies' :: (Ord b, Num b) => [b] -> b -> [Bool]
candies' lst e = map (>= (maximum lst)) $ (map (+e) lst)


-- 1512. Number of Good Pairs
nums = [1,2,3,1,1,3]

-- myFunc lst = groupBy (\x y -> (snd x == snd y) ) (sortBy (compare `on` snd) $ zip [0,1..] (lst)) 
zippertup lst = zip [0,1..] (lst)

-- taker tpl lst = filter ((< (tpl.fst)) . fst) lst
tup = [(0,1),(3,1),(4,1)]
example tp lst = filter ((==(snd tp)).snd) lst

tupleFilter tup lst = filter ((==(snd tup)).snd) lst

taker [] = []
taker [x] = [x]
taker (x:y:xs)
  | (fst x < fst y) = x : taker (y:xs)
  | otherwise = taker (y:xs)

eqvalsmlleridx tuplst = [ x | x <- tuplst , y <- tuplst, (fst x < fst y) && (snd x == snd y)] 
solution lst = length $ eqvalsmlleridx (zip [0,1..] lst)


testcomp = [ x + y | x <- [1..10]  , y <- [50..60] ]


hhh ys xs = reverse ys ++ reverse xs
ttt ys xs = reverse (xs++ys)

testcasea = [1,2,3,1,1,3]

maplength lst = fmap length . group . sort $ lst 
-- pairs appears to be logarithmic function
-- https://www.google.com/search?newwindow=1&hl=en&sxsrf=ALeKk02YAXV_sC1nIuJw1LmkBSuRqpYeig%3A1603823528984&ei=qGeYX8nWO8Pk9APOmZCwCA&q=x+*+%28x-1%29+%2F+2&oq=x+*+%28x-1%29+%2F+2&gs_lcp=CgZwc3ktYWIQAzIJCAAQyQMQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeMgYIABAWEB4yBggAEBYQHjIGCAAQFhAeOgQIABBHOgQIIxAnOgYIABAKEEM6BAguEEM6BQgAEJECOgUIABCxAzoECAAQQzoICAAQsQMQgwE6CQgAEMkDEAoQQzoKCC4QsQMQFBCHAjoNCAAQsQMQgwEQFBCHAjoCCAA6BQguELEDOgsIABCxAxCDARDJAzoHCAAQFBCHAjoECAAQHlCWiQFYo_EBYJ_0AWgAcAR4AIABd4gBpAqSAQM2LjeYAQCgAQGqAQdnd3Mtd2l6yAEHwAEB&sclient=psy-ab&ved=0ahUKEwjJyuidtNXsAhVDMn0KHc4MBIYQ4dUDCA0&uact=5
pairs = (\x -> div (x * (x-1)) 2)
amountofpairs lst = sum . fmap (\x -> div (x * (x-1)) 2) . fmap length . group . sort $ lst

-- Defanging an IP Address
-- A defanged IP address replaces every period "." with "[.]".

-- defang :: [Char] -> [Char]
-- defang lst = map (\x -> ) lst
charToString c = c:[]
cnvrt ls = map charToString ls
dfg :: [Char] -> [Char]
dfg ls = concat $ map (\x ->  if x == "." then "[.]" else x) (cnvrt ls)

-- 771. Jewels and Stones
-- sj :: 
sj j s = foldr (\c acc ->  if (c `elem` j) then (acc+1) else acc ) 0 s 


-- s = "codeleet", indices = [4,5,6,7,0,2,1,3]
aa = "codeleet"
bb = [4,5,6,7,0,2,1,3]
shfla s idxs = map (fst) $ sortBy (compare `on` snd) (zip s idxs) 
shflb s idxs = map (snd) $ sort (zip idxs s) 

-- :! cls

-- Type Signature for stringify visualization
-- stringify :: [Char] -> [[Char]]
-- the stringify fn we created will take a list and stringify each character.
stringify str = map(:[]) str
-- Type Signature for defang visualization
-- defang :: [Char] -> [Char]
-- the defang fn we created will now take the stringified list of all characters
-- handle and lastly concat the string.
defang isp = concat(map(\s -> if s == "." then "[.]" else s) (stringify(isp)))

dfng' str = concat(map(\s -> if s == "." then "[.]" else s) (map pure str) ) 

-- *Main> defang "ha.ku.na.ma.ta.taaaa"
-- will return -> "ha[.]ku[.]na[.]ma[.]ta[.]taaaa"

-- // Solution V1
-- // Here we split on the .
-- // And then join the "[.]"
-- const dissect = address => {
-- return address.split('.').join('[.]');
-- };

-- // Solution V2
-- // Here we are using Regex (regular expression)
-- // We start the expression with '/'
-- // indicating the start of our Regex
-- // Then we use '\.' this is a Escape sequence
-- // it can be used to insert reserved, special, and unicode characters.
-- // All escaped characters begin with the \ character.
-- // We are targeting the '.' => '\.'
-- // Lastly we flag '/g' the end of the expression with 'g'
-- // Applying it globally.
-- // "[.]" indicates that we will replace all . with the literal string [.]


-- const dissect = address => {
-- return address.replace(/\./g,"[.]");
-- };

aaa = "codeleet"
bbb = [4,5,6,7,0,2,1,3]

shuffle' :: (Ord a, Ord b) => [a] -> [b] -> [b]
shuffle' s i = map (snd) (sort (zip s i))
-- [('c',4),('o',5),('d',6),('e',7),('l',0),('e',2),('e',1),('t',3)]
-- shfl' :: (Ord a, Ord b) => [a] -> [b] -> [b]
-- shfl' = ((map snd . sort) . ). zip



dfng'' str = concat(map(\s -> trace ("map 1 ++ " ++ show s) (if s == "." then "[.]" else s)) (map (trace ("map 2" ) . pure) str) )

nums' = [8,1,2,2,3]
-- Output: [4,0,1,1,3]


-- 1365. How Many Numbers Are Smaller Than the Current Number
smlrNmsSol ns = map (smlrNms ns) ns
smlrNms ns x = length $ filter (<x) ns

-- digits nms = map (read . (:[])) . show $ nms

-- 1342. Number of Steps to Reduce a Number to Zero

stpsz 0 s = s 
stpsz n s 
  | (even n) = stpsz (n `div` 2) (s+1) 
  | otherwise = stpsz (n - 1) (s+1)

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

-- 1281. Subtract the Product and Sum of Digits of an Integer

-- dps n = abs ((product ) - (sum n))

