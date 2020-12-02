import Data.List.Split

toNum :: [String] -> [Int]
toNum = map read 

bounds str = toNum $ splitOn "-" $ (!!) (clean str) 0
ltr str = head $ take 1 $ (!!) (clean str) 1
psswrd str = (!!) (clean str) 2

solveOne str = solve
  where bnds = bounds str 
        letter = ltr str 
        password = psswrd str 
        count = length $ filter (==letter) password 
        solve = count >= (head bnds) && count <= (last bnds)
 
main = do 
  listof <- lines <$> readFile "adventofcode//day2data.txt"
  let passwordCount = length $ filter (solveOne) listof
  print passwordCount
  return ()
