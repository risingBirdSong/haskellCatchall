import qualified Data.List

alice = [1,2,3]
bob = [3,2,1]
tripletCompare (x,y) (a,b)
  | x > y = (succ a, b)
  | x < y = (a, succ b)
  | otherwise = (a,b)

-- compareTriplets a b = pairToList $ tripletCompare (Data.List.zip a b) (0,0)
compareTriplets a b = pairToList $ foldr tripletCompare (0,0) $ zip a b 
pairToList (a,b) = [a,b]
