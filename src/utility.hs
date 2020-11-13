takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

test = takeUntil (==1) [2,4,1,3]