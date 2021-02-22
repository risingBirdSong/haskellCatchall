import Data.List 
import Data.Char
myMatrix = [
    [1,3,2,4,5],
    [6,7,9,2,3],
    [9,8,4,5,6],
    [0,4,1,7,8]
  ]
myDiag n mtrx = go (n) (transpose $ take n mtrx)
    where go 0 _ = [] 
          go n (r:rs) = indx (n-1) r : go (n-1) rs 
              
-- caller mtrx = transpose mtrx

indx n xs
      | n >= ln || n < 0 = Nothing 
      | otherwise = Just (xs !! n)
    where ln = length xs 

-- diags 