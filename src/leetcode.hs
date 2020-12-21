a = [1,1]
b = [2,2]
-- fairCandy as bs = [small, big]
      -- where small = 

smaller as bs = head $ filter (\x -> (sum x) == (min (sum as) (sum bs))) [as,bs]

smaller_ as bs 
  | sum as < sum bs = as 
  | otherwise = bs 

