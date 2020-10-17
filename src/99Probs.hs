-- prob 1
-- Find the last element of a list.
-- no built in 
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
