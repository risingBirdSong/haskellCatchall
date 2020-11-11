-- 1047. Remove All Adjacent Duplicates In String
import Data.List.Split
import System.Random
import Control.Monad
import Control.Applicative
import Data.Function
import Data.List
import Test.QuickCheck
import Data.Maybe
-- import qualified Data.Text as T
import Data.Bool
import Debug.Trace
import qualified Data.Set as Set



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

dps n = abs ((product dgs) - (sum dgs))
    where dgs = digits n


avg :: (Fractional a, Foldable t) => t a -> a
avg nums = (sum nums) / realToFrac (length nums)

-- https://stackoverflow.com/questions/12636613/how-to-calculate-moving-average-without-keeping-the-count-and-data-total
rngAvg avg smpl n = avg + (second - first)
    where first =  (avg /  n)
          second = (smpl / n)    

avgDiv avg n = (avg / n)
smplDiv avg smpl n=  (smpl / n)


-- pascltri = [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]


-- pscl (lst) acc lvl fnsh 
--   | (lvl == fnsh) = []
--   | (lvl < fnsh) = [1] : pscl (lst) (lvl+1) fnsh


btwnnums = [10,5,15,3,7,0,18]
-- 938. Range Sum of BST
btwn lwr upr lst =  sum $ filter (\x -> (x <= upr) && (x >= lwr)) lst 

-- 1588. Sum of All Odd Length Subarrays
arr1 = [1,4,2,5,3]

oddsubs ar = [ sum (xs) | xs <- permutations ar , odd (sum xs) == True ]

powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

powerset' = filterM (const [True,False])


-- *Main> sort (subsequences [1,2,3,4])
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,4],[1,3],[1,3,4],[1,4],[2],[2,3],[2,3,4],[2,4],[3],[3,4],[4]]

-- *Main> sort (powerset [1,2,3,4])    
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,4],[1,3],[1,3,4],[1,4],[2],[2,3],[2,3,4],[2,4],[3],[3,4],[4]]


-- idiomatic subsequences

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- >>> subsequences "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
subsequences'            :: [a] -> [[a]]
subsequences' xs         =  [] : nonEmptySubsequences' xs

-- | The 'nonEmptySubsequences'' function returns the list of all subsequences' of the argument,
--   except for the empty list.
--
-- >>> nonEmptySubsequences' "abc"
-- ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences'         :: [a] -> [[a]]
nonEmptySubsequences' []      =  []
nonEmptySubsequences' (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences' xs)
  where f ys r = ys : (x : ys) : r

gatherodd nums = (filter (\x -> odd ((length x))) ( subsequences nums))

ex1' = [(0,1),(2,2),(4,3)] -- False
ex2' = [(0,1),(1,2),(2,3),(3,6)] -- False

contigcheck :: (Num a, Ord a) => [(a,a)] -> Bool
contigcheck [] = True
contigcheck [x] = True
contigcheck (x:y:xs)
  | ((fst (x)+1)  == fst (y)) = contigcheck (y:xs)
  | otherwise = False

  -- [1,4,2,5,3]
contigs nums = sum $ map sum $ filter (odd . length) $ map (map (snd)) $ (filter (contigcheck) (subsequences (zip [0,1..] nums)))

output1 = [[],[(0,1)],[(1,4)],[(0,1),(1,4)],[(2,2)],[(1,4),(2,2)],[(0,1),(1,4),(2,2)],[(3,5)],[(2,2),(3,5)],[(1,4),(2,2),(3,5)],[(0,1),(1,4),(2,2),(3,5)],[(4,3)],[(3,5),(4,3)],[(2,2),(3,5),(4,3)],[(1,4),(2,2),(3,5),(4,3)],[(0,1),(1,4),(2,2),(3,5),(4,3)]]
output2 = [[],[1],[4],[1,4],[2],[4,2],[1,4,2],[5],[2,5],[4,2,5],[1,4,2,5],[3],[5,3],[2,5,3],[4,2,5,3],[1,4,2,5,3]]


sumOdd :: [Int] -> Int
sumOdd x = sum $ sum <$> filter (odd . length) (filter (flip isInfixOf x) (subsequences x))

numToString num =  (chunksOf 1) (show num)

tnums = [12,345,2,6,7896] 
evenLengthDgts nums = length $ filter (even . length) $ map (numToString) nums

-- mtrx : [[Int]]

data MaybeCoords a = Falsy | Jst a 
-- testing (MaybeCoords x) (MaybeCoords y) 
--   | (x + y) = Jst (x + y)
--   | otherwise = Falsy


-- grtstReWrite a b = 

tsttst = (max) <$> Just 2 <*> Just 8 

points = [[1,1],[3,4],[-1,0], [1,1]]

grtst a b = max (abs (x2 - x1))(abs (y2 - y1))   
  where x1 = head a
        x2 = head b
        y1 = last a
        y2 = last b
lstTime coords = sum $ concat (f coords)
f :: (Num a, Ord a) => [[a]] -> [[a]]
f (x:y:xs) = [(grtst x y)] : f (y:xs)   
f (x:xs) = []   



ooo = [[3],[4],[0]]


subs = filterM (const [True, False])

-- 1572. Matrix Diagonal Sum

-- mrdx :: [[Int]] -> [[Int]]
mymtrx = chunksOf 3 [1,2,3,4,5,6,7,8,9]
diags1 = diagonals $ transpose $ chunksOf 3 [1,2,3,4,5,6,7,8,9]
diags2 = diagonals $ map (reverse) $ transpose $ chunksOf 3 [1,2,3,4,5,6,7,8,9]

-- makemtrx n =  

-- chunksOf 3 [1,2,3,4,5,6,7,8,9]
-- [[1,2,3],
--  [4,5,6],
--  [7,8,9]]

-- [[1,4,7],
--  [2,5,8],
--  [3,6,9]]

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

-- great resource!
-- https://www.fpcomplete.com/haskell/library/containers/
-- 1436. Destination City

-- λ> let fourCorners = Set.fromList [Arizona, NewMexico, Colorado, Utah]
-- λ> let borderStates = Set.fromList [California, Arizona, NewMexico, Texas]
-- λ> Set.union fourCorners borderStates
-- fromList [Arizona,California,Colorado,NewMexico,Texas,Utah]
-- λ> Set.intersection fourCorners borderStates
-- fromList [Arizona,NewMexico]
-- λ> Set.difference fourCorners borderStates
-- fromList [Colorado,Utah]
-- λ> Set.difference borderStates fourCorners
-- fromList [California,Texas]
-- λ> let symmetricDifference a b = Set.union a b Set.\\ Set.intersection a b
-- λ> symmetricDifference fourCorners borderStates
-- fromList [California,Colorado,Texas,Utah]

paths = [["London","New York"],["New York","Lima"],["Lima","Sao Paulo"], ["New York","London"]]
destc cts = arriv `Set.difference` depart where 
  depart = Set.fromList $ map (head) cts
  arriv = Set.fromList $ map (last) cts

dprts cts = Set.fromList $ map (head) cts
arrvs cts = Set.fromList $ map (last) cts


-- https://www.youtube.com/watch?v=J5IIH35EBVE&t=125s&ab_channel=NateSantti
-- Hao sent me a nice explanation of the c

-- -123

--check is neg or pos?
-- return with the same value

-- convert the num into a string... 
-- split the string on every charact -> "1,2,3"
-- reverse that -> "3,2,1,-"
--join -> "321"
-- convert to a number
revNum :: Int -> Int
revNum num 
  | (num > 0) = logic num
  | (num < 0) =  (logic (num * (-1))) * (-1)
    where logic input = read (concat (reverse (chunksOf 1 (show input))))


mat = [[1,2,3],
      [4,5,6],
      [7,8,9]]



-- bs stands for backslask because the matrix from top left to bottom right looks like backslash pattern
-- fs stands for forward slash
diags_ _ [] _ = []
diags_ dir (l:mtr) i = [l !! i] : diags_ dir (mtr) (i+dir) 

diagsolve mtrx = first + second
  where first = sum $ concat $ diags_ (1) mtrx (0)
        second = sum $ concat $ diags_ (-1) mtrx ((length mat)-1)


-- 1304. Find N Unique Integers Sum up to Zero

-- 728. Self Dividing Numbers
-- sdn num = [ n | n <- [1..num], ]
componentA = fst ([2,8],28)
componentB = all (\x -> x /= 0) [1,2,3,0]
solvedSelfDiv = selfDivSolution 100
-- [1,2,3,4,5,6,7,8,9,11,12,15,22,24,33,36,44,48,55,66,77,88,99]

selfDivSolution n =
  firsts $
  findSelfDivs $
  rejectZeros $
  zip [1..n] (map (numSplt) [1..n])

firsts = map fst
findSelfDivs = filter (\tpl -> selfDiv (fst tpl) (snd tpl))
-- so that we can safely divide by each numbers... get rid of zeroes bcz zeroes would break
rejectZeros :: (Foldable t, Eq a1, Num a1) => [(a2, t a1)] -> [(a2, t a1)]
rejectZeros lstTupl = filter (\tpl -> all (\x -> x /= 0) (snd tpl)) lstTupl

selfDiv num [] = True 
selfDiv num (d:digits) 
  | (num `mod` d == 0) = selfDiv num digits
  | otherwise = False

numSplt str = map (\x -> read x :: Integer) $ mysplit (show str) 

mysplit source = mysplit' source []
mysplit' [] out = out
mysplit' (s:src) out = mysplit' src (out ++ [[s]])

-- myNumSplt n = myNumSplt' n [] 
-- myNumSplt' :: (Integral a) => a -> [a] -> [a]
myNumSplt n = myNumSplt' n [] 
myNumSplt' 0 out = out
myNumSplt' num out = myNumSplt' (div num 10) (mod num 10 : out)

digts :: Int -> [Int]
digts 0 = []
digts x = d : digts ds
  where (ds, d) = divMod x 10

testa_divmod =  divMod 12 10
-- (1,2)

-- 905. Sort Array By Parity

paritysort nums = concat $ paritysort' nums [] []
paritysort' [] e o = [e,o]
paritysort' (n:nums) e o
  | even n = paritysort' nums (n:e) o
  | otherwise = paritysort' nums e (n:o)

selfDividing :: Int -> Int -> [Int]
selfDividing start stop = filter (liftM2 all divides digits_) [start..stop]

divides :: Int -> Int -> Bool
divides num 0 = False
divides num base = num `mod` base == 0

digits_ :: Int -> [Int]
digits_ 0 = []
digits_ x = d : digits_ ds
  where (ds, d) = divMod x 10

mymain = print $ selfDividing 1 22

sortArrayByParity = uncurry (++) . partition even 


onlyevens nums = filter (even) nums
-- allTest lst = filter (liftM2 all even (>10)) lst 
testing nums = filter (>50) $ filter (even) nums


divTest n = all (divides n) (digits_ n)

vvv = [3,4,-1,1]


getready nums = filter (>0) $ sort nums 
ready = [1,3,4]
xxx [x] = []
xxx (x:y:lst) 
  | (x+1 /= y) = x : xxx (y:lst)
  | otherwise = xxx (y:lst)

-- findGap :: :: (Num b, Enum b) => [a] -> [(a2 -> b -> c) -> (a2, b) -> c]
findGap nums = snd . head $filter (uncurry (/=)) $ zip nums ([(head $filter (>0) $ sort nums)..])

eee nums = ([(head $filter (>0) $ sort nums)..])

zipped :: (Num b, Enum b) => [a] -> [(a, b)]
zipped  nums = zip nums [1..] 