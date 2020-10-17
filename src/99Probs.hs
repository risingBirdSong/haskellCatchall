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