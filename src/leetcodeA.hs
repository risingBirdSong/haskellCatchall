import Data.List 

clmbA =  [1 , 100, 1, 1, 1, 100, 1, 1, 100, 1]

climbing [] steps = []
climbing [cur] steps = [cur]
climbing (cur:[x]) steps = [cur]
climbing (cur:x:y:zs) steps 
  | x < y =  cur : climbing (x:y:zs) (steps + 1)
  | x >= y = cur : climbing (y:zs) (steps + 1)

climbSolve (path) = minimum $ map (sum) [(climbing (drop 1 path) 0) , (climbing path 0)] 