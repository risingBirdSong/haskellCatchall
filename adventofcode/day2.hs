import Data.List.Split

toNum :: [String] -> [Int]
toNum = map read 

validPassword str = solve
  where bnds = toNum $ splitOn "-" $ (!!) (words str) 0
        letter = head $ take 1 $ (!!) (words str) 1
        password = (!!) (words str) 2
        count = length $ filter (==letter) password 
        solve = count >= (head bnds) && count <= (last bnds)  
      
main = do 
  listof <- lines <$> readFile "adventofcode//day2data.txt"
  let passwordCount = length $ filter ( validPassword) listof
  print passwordCount
  return ()








bounds str = toNum $ splitOn "-" $ (!!) (words str) 0
ltr str = head $ take 1 $ (!!) (words str) 1
psswrd str = (!!) (words str) 2


solveOneA str = solve
  where bnds = bounds str 
        letter = ltr str 
        password = psswrd str 
        amatch = (!!) password (head bnds -1) == (letter)
        bmatch = (!!) password (last bnds -1) == (letter)
        truthcount = length $ filter (\x -> x == True) [amatch, bmatch]
        solve = truthcount == 1