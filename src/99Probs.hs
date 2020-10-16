-- prob 1
-- Find the last element of a list.
-- no built in 
sol1 :: [a] -> Maybe a
sol1 [] = Nothing 
sol1 (x:xs) = sol1' x xs  
sol1' :: t -> [t] -> Maybe t
sol1' x [] = Just (x)
sol1' x (y:ys) = sol1' y ys 