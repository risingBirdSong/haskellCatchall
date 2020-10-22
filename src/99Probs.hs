-- prob 1
-- Find the last element of a list.
-- no built in 
import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad
import System.Random
import Universum.Nub
import Data.Function
import Data.Map  (fromListWith, toList)


sol1 :: [a] -> Maybe a
sol1 [] = Nothing 
sol1 (x:xs) = sol1' x xs  
sol1' :: t -> [t] -> Maybe t
sol1' x [] = Just (x)
sol1' x (y:ys) = sol1' y ys 

-- built in
sol1a [] = Nothing
sol1a xs = Just (last xs)

fld ls = foldr1 (const id) ls
fld1 = foldl (const id) 0 [1,2,3]
hm x  = (const id) x

alwaysFirst = foldr1 (const) [1,2,3] -- 1
alwaysLast = foldr1 (flip const) [1,2,3] --3

myLast'''' xs = foldl1 (curry snd) xs

-- so 
-- acc cur
-- 0    1
-- 0    2
-- 0    3
-- and (const id) will always select the 2nd of those
-- the last of which is 3, so thats how the whole expression evaluates to 3


-- Problem 2
-- (*) Find the last but one element of a list.

sndLast [] = Nothing
sndLast (x:y:z:ls) = sndLast (y:z:ls)
sndLast (x: _) = Just (x)

sndLast_a xs = head $ tail (reverse xs)

sndLast_b xs = last . init $ xs

myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs


lastbut1safe :: Foldable f => f a -> Maybe a
lastbut1safe = fst . foldl (\(a,b) x -> (b,Just x)) (Nothing,Nothing)

exprmt :: Foldable f => f a ->(Maybe a, Maybe a)
exprmt = foldl (\(a,b) x -> (b,Just x)) (Nothing,Nothing)

-- $ is for applying a function to a value, . is for smashing two functions together.

-- no built ins
elementAt [] i = Nothing 
elementAt (x:xs) i 
  | i == 1 = Just (x)
  | otherwise = elementAt xs (i - 1)
  

zipTillN list n = zip [1..n] list

elemAt'' x lst = fst . last $ zip [1..x] lst

elemat_d x lst = head $ drop (x-1) lst

elemat_t x lst = last $ take x lst

-- standard built in 
-- [1,2,3] !! 1 -- 2

-- Problem 4
-- (*) Find the number of elements of a list.

countEle lst = countEle' lst 0 
countEle' [] c = c 
countEle' (x:xs) c = countEle' xs (c+1)

cnt lst = fst . last $ zip [1..] lst

cnt' [] = 0
cnt' (x:xs)=  1 + cnt' xs  

examineFold_r_x lst = foldr (\x y -> x) 0 lst -- for foldr the first arg in the lambda is cur, the second is accu 
examineFold_r_y lst = foldr (\x y -> y) 0 lst -- for foldr the first arg in the lambda is cur, the second is accu 
examineFold_l_x lst = foldl (\x y -> x) 0 lst -- for foldl the first arg to lamba is acc, the second is current
examineFold_l_y lst = foldl (\x y -> y) 0 lst -- for foldl the first arg to lamba is acc, the second is current

cntFold lst = foldr (\x y -> y+1) 0 lst 

cntMap lst = sum ( map (\_->1) lst)

-- Problem 5
-- (*) Reverse a list.



rev [] = []
rev (x:xs) = rev xs <> [x]

rev lst = rev' lst []
rev' [] ys = ys
rev' (x:xs) ys = rev' (xs) (x : ys )

-- cool the one I wrote is also written in the examples and they say this about it -> (they did with where syntax)
-- However this definition is more wasteful than the one in Prelude as it repeatedly reconses the
-- result as it is accumulated.  The following variation avoids that, and thus computationally 
-- closer to the Prelude version.

rvrs :: [a] -> [a]
rvrs [] = []
rvrs (x:xs) = rvrs xs ++ [x]

foldrev lst = foldr (\x acc -> (acc ++ [x])) [] lst

data NestedList a = Elem a | List [NestedList a]

tst = (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) 


-- how do I guard with custom types like ?
guardTypes (Elem x) = "elem"
guardTypes (List x) = "list of elem"

flttn (Elem x) = [x]
flttn (List x) = concat . map flttn $ x

flt1 = flttn tst -- [1,2,3,4,5]

-- guardTypes (Elem 3) "elem"
-- guardTypes (List [Elem 3]) "list of elem"

-- flatten lst 
--   | 

-- flattened = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) 


-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).


pali xs = xs == (reverse xs)

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

boxer [] = []
boxer (x:xs) = [x] : boxer xs

boxer' [] = []
boxer' (x:xs) = boxer' xs ++ [x]
--boxer [1,2,3] [3,2,1]
boxer'' [] = []
boxer'' (x:xs) = boxer'' xs ++ [[x]]



pack ls = pack' ls []
pack' [] _ = []
pack' [l] sub = [l:sub]
pack' (x:y:ls) sub
  | x == y = pack' (y:ls) (x:sub)
  | x /= y = (x:sub) : pack' (y:ls) []

pack'' [] = []
pack'' (x:xs) = (x : l) : pack'' r
  where
    (l, r) = span (== x) xs

simpleSpan (x:xs) = span (==x) xs
-- simpleSpan [1,1,1,2,2,3] ([1,1],[2,2,3])

grouped xs = group xs
-- grouped [1,1,1,2,2,3] -> [[1,1,1],[2,2],[3]]

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called
--  run-length encoding data compression method. Consecutive duplicates of elements are encoded
  -- as lists (N E) where N is the number of duplicates of the element E.

encode xs = map length (group xs)

encode' ys = (\x -> (length x, x)) <$> ( group ys)
-- encode' [1,1,1 ,2,2,3] -> [(3,[1,1,1]),(2,[2,2]),(1,[3])]

endcodeMap ys = map (\x -> (length x, head x)) (group ys)

-- encode_ :: String -> [(Int, Char)]
encode_ x = (\(x:xs) -> (1 + length xs, x)) <$> group x

-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

data M_S a = Single (Int, a) | Multiple (Int, a) deriving (Show, Eq, Read)

myS = Single (4,2)
myMult = (Multiple (3,1))

data Customized a = MyCustomTuple (Int, Int) deriving (Show, Eq, Read)

ttt = MyCustomTuple (3,1)



-- handler :: [b] -> M_S (Int, b)
handler x
  | length x > 1 = (Multiple (length x, head x))
  | otherwise  =  (Single(1, head x))

extracter (Multiple x) = fst (x)
extracter (Single x) = snd (x)

multsingle xs = map handler $ group xs

-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
-- Example in Haskell:
-- λ> decodeModified 
      --  [Multiple 4 'a',Single 'b',Multiple 2 'c',
        -- Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

-- decode xs = map 
--   | x == Multiple a = replicate (fst a) (snd a)
--   | == Single a = replicate (1) (snd a)

decode' (Multiple a) = replicate (fst a) (snd a)
decode' (Single a) = [snd a]
decode xs = concat  (map decode' xs)

data1 = [Multiple (4, 'a'), Single (1, 'b'),Multiple (2 ,'c'), Multiple (2 ,'a'),Single (1, 'd'),Multiple (4 , 'e')]
-- decode data1 -> "aaaabccaadeeee"
data2 = [Multiple (4,2), Multiple (5,3), Single (1,1)]
-- decode data2 -> [2,2,2,2,3,3,3,3,3,1]

-- Problem 14
-- (*) Duplicate the elements of a list.

duplicate xs = concatMap (replicate 2) xs

-- what is uncurry like?
testa = foldr (+) 0 [1,2]
testUnCurry = uncurry (+) (1,2)
testUnCurryA = map (uncurry (max)) [(1,2),(3,4),(7,8)] -- [2,4,8]

currTest = curry fst 1 2

dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' list = concat [[x,x] | x <- list]

dupli'' xs = xs >>= (\x -> [x,x])

exampleMap = map (replicate 2) [1,2,3] -- [[1,1],[2,2],[3,3]]
exampleCMap = concatMap (replicate 2) [1,2,3] -- [[1,1],[2,2],[3,3]]
exampleMonad = replicate 2 =<< [1, 2, 3] -- [1,1,2,2,3,3]

dupli''' = (<**> [id,id])
 
dupli'''' xs  = concatMap (\x->[x,x]) xs

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.

repli_ tms v = concat (map (\_->[v])  [1..tms])
repli xs tms = concatMap (repli_ tms) xs

repli' :: [a] -> Int -> [a]
repli' [] _ = []
repli' (x:xs) n = foldr (const (x:)) (repli' xs n) [1..n]

-- Problem 16
-- (**) Drop every N'th element from a list.

indexed xs = zip [0,1..] xs
dropN xs n = map (snd) (filter (\x -> fst x `mod` n /= 0)( zip [1..] xs))

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
-- split "abcdefghik" 3
-- ("abc", "defghik")
splitGroup xs n = map (reverse) (last (splitGroup' xs [] [] n 0))
splitGroup' [] _ _ _ _ = []
splitGroup' (x:xs) prt prta n c
  | c < n = [(x:prt) , xs] : splitGroup' (xs) (x:prt) prta (n) (c+1)
  | c >= n = [prt, (x:prta)] : splitGroup' (xs) (prt) (x:prta) (n) (c+1) 

spltAtIt xs n = (take n xs, drop n xs)

-- Problem 18
-- (**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th
--  element of the original list (both limits included). Start counting the elements with 1.

mySlice str stp lst = take (stp-str) (drop str lst)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _  = []
slice (x:xs) i k
 | i > 1      = slice xs (i - 1) (k - 1)
 | k < 1      = []
 | otherwise  = x:slice xs (i - 1) (k - 1)

rotateN n lst = b ++ f
  where f = take n lst
        b = drop n lst

rotate n xs = take len . drop (n `mod` len) . cycle $ xs
  where len = length xs

rotate' n xs  = take (length xs) $ drop (length xs + n) $ cycle xs

rotate'' :: Int -> [a] -> [a]
rotate'' _ [] = []
rotate'' n xs = zipWith const (drop n (cycle xs)) xs

-- zipWith const ([1..10])([10..1])

-- Problem 20
-- (*) Remove the K'th element from a list.

removeKth k lst = f ++ b
  where f = take (k -1) lst
        b = drop k lst

removeKth' k lst = (map (snd))  (filter (\x -> fst x /= k ) (zip [0,1..] lst))

removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
  where (l, r) = removeAt (n - 1) xs
  
removeAt' :: Int -> [a] -> (a, [a])
removeAt' n xs = let (front, back) = splitAt n xs in (last front, init front ++ back)

removeAt'' n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)


-- Problem 21
-- Insert an element at a given position into a list.

insertAt x n ls = f ++ [x] ++ b
      where f = take n ls
            b = drop n ls

nsert :: a -> [a] -> Int -> [a]
nsert x ys     1 = x:ys
nsert x (y:ys) n = y:nsert (x) (ys) (n-1)

-- Problem 22
-- Create a list containing all integers within a given range.

cheekyRange x y = [x..y]

range_r x y 
  | x >= y = []
  | otherwise = x:range_r (x+1) y

rnd_select_my n lst = do
  g <- getStdGen
  print (take n (randomRs ('a','g') g :: [Char]))

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- newStdGen
  return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

addUp n 
  | n == 0 = 0
  | otherwise = n + addUp (n-1)



diff_select :: Int -> Int -> IO [Int]
diff_select n m = take n . nub . randomRs (1, m) <$> newStdGen

diff_select_r n m = take n . nub . randomRs (1,m) <$> newStdGen 

-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes 
-- the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

-- combinations 3 "abcdef"
-- ["abc","abd","abe",...]

combinations n lst = replicateM n lst

rplcM_ :: (Ord t, Num t, Applicative f) => t -> f a -> f [a]
rplcM_ n lst =
    loop n
  where
    loop n
      | n <= 0 = pure []
      | otherwise = liftA2 (:) lst (loop (n - 1))

replicate'' :: (Ord t, Num t) => t -> a -> [a]
replicate'' n x  
    | n <= 0    = []  
    | otherwise = x:replicate'' (n-1) x




-- comboPeople n xs = combos  

combos :: [[a]] -> [[a]]
combos [] = [[]]
combos ([]:ls) = combos ls
combos ((h:t):ls) = map (h:) (combos ls) ++ combos (t:ls)

threePeople = ["aldo","beat","carla"]      
fourPeople = ["aldo","beat","carla", "david"]      
allPeople = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

basegroup n xs =  map (ordNub)  $ replicateM n xs
-- filter ((==n) . length )
sortsubs  xs n =  ordNub (filter ((==n) . length ) ( map sort  (map (ordNub)  $ replicateM n xs) ))
-- grouponLength l n xs = filter ((==l) . length ) (map sort) $ map (ordNub)  $ replicateM n xs
-- wait so how compose sortsubs so that I can collect over multiple nums of lengths of people groupings?
-- composegroup lngs xs = foldr (sortsubs xs) [] lngs   

composecombos [] people = [] 
composecombos (l:lngs) people = (sortsubs people l) : composecombos lngs people


listadder xs = foldr (++) [] xs 
numadder xs = foldr (+) 0 xs

reslen2 = [["aldo","beat"],["aldo","carla"],["beat","aldo"],["beat","carla"],["carla","aldo"],["carla","beat"]]
resa =[["aldo","beat","carla"],["aldo","beat","david"],["aldo","beat","evi"],["aldo","beat","flip"],["aldo","beat","gary"]]
resb = [["aldo","beat","carla","david"],["aldo","beat","carla","evi"],["aldo","beat","carla","flip"],["aldo","beat","carla","gary"],["aldo","beat","carla","hugo"]]
-- resa ++ resb ->
appendeda_b = [["aldo","beat","carla"],["aldo","beat","david"],["aldo","beat","evi"],["aldo","beat","flip"],["aldo","beat","gary"],["aldo","beat","carla","david"],["aldo","beat","carla","evi"],["aldo","beat","carla","flip"],["aldo","beat","carla","gary"],["aldo","beat","carla","hugo"]]

-- https://wiki.haskell.org/99_questions/Solutions/27
combination' :: Int -> [a] -> [([a],[a])]
combination' 0 xs     = [([],xs)]
combination' n []     = []
combination' n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination' (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination'  n    xs ]


re_combo :: Int -> [a] -> [([a], [a])]
re_combo 0 xs = [([],xs)]
re_combo n [] = []
re_combo n (x:xs) = ts ++ ds
  where
 ts = [(x:ys,zs) | (ys,zs) <- re_combo (n-1) xs] 
 ds = [(ys, x:zs) | (ys,zs) <- re_combo n xs]

 
group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n:ns) xs =
    [ g:gs | (g,rs) <- combination' n xs
           ,  gs    <- group' ns rs ]

re_group :: [Int] -> [a] -> [[[a]]]
re_group [] _ = [[]]
re_group (n:ns) xs =
    [ g:gs | (g,rs) <- re_combo n xs, gs <- re_group (ns) rs]


answercomboa = combination' 4 fourPeople -- [(["aldo","beat","carla","david"],[])]
myAttempta = sortsubs fourPeople 4 -- [["aldo","beat","carla","david"]]


myAttemptb =  composecombos [2,3] fourPeople -- [[["aldo","beat"],["aldo","carla"],["aldo","david"],["beat","carla"],["beat","david"],["carla","david"]],[["aldo","beat","carla"],["aldo","beat","david"],["aldo","carla","david"],["beat","carla","david"]]]
-- answergroupa = 

-- Problem 28
-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

-- Example:

-- * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
-- Example in Haskell:

-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]

test_lsort =  ["abc","de","fgh","de","ijkl","mn","o"]
lsort xs = sortBy (compare `on` length) $ sort xs
-- ["o","de","de","mn","abc","fgh","ijkl"]



pureNumAppend =  (pure 1) <> (pure 2) :: [Int]  -- [1,2]

-- composedSort = sortBy $ (C.comparing length) <> compare

-- b) Again, we suppose that a list contains elements that are lists themselves.
--  But this time the objective is to sort the elements of this list according to their length frequency;
--   i.e., in the default, where sorting is done ascendingly,
--  lists with rare lengths are placed first, others with a more frequent length come later.


-- hand made option from SO
-- https://stackoverflow.com/questions/13517114/count-frequency-of-each-element-in-a-list
freq s = map (\x -> (head x, length x)) . group . sort $ s
  
-- g = map (head &&& length) . group . sort     -- without the [...]
-- https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs
frequency xs = toList (fromListWith (+) [(x, 1) | (x)  <- xs])


lengthmap xs = map (\x -> (length x , x, 0)) xs

-- lfsort :: [(a, Int)] -> [Int]
lfsorta xs =  frequency  xs
lfsortb xs = sortOn (\x -> snd x) $ frequency  xs

lengthFreq [] = []
lengthFreq (x:xs) = []

--  lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

ddd = ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
ooo = [("abc",1),("de",2),("fgh",1),("ijkl",1),("mn",1),("o",1)]

grpByLength xs = concat . lsort . groupBy equalLength $ lsort xs
equalLength xs ys = length xs == length ys

-- lfsort_ :: [[a]] -> [[a]]
lfsort_ xs = concat . lsort $ groupBy ((==) `on` length) $ lsort  xs
-- lfsort_ = concat . lsort $ groupBy ((==) `on` length) . lsort


solution :: Integer -> Integer
solution number = if number < 0 
                  then 0 
                  else sum[x| x <- [1..number-1], (x `mod` 3 == 0 || x `mod` 5 == 0)]


solution n 
  | n <= 0 = 0
  | (n `mod` 3 == 0 || n `mod` 5 == 0) = n + solution (n-1)
  | otherwise = solution (n-1)