import qualified Data.List

alice = [1,2,3]
bob = [3,2,1]
tripletCompare [] acc = acc 
tripletCompare ((x,y):xs) (a,b)
  | x > y = tripletCompare xs (succ a, b)
  | x < y = tripletCompare xs (a, succ b) 
  | x == y = tripletCompare xs (a,b)
compareTriplets a b = pairToList $ tripletCompare (Data.List.zip a b) (0,0)
pairToList (a,b) = [a,b]
