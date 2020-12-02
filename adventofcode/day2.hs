import Data.List.Split

toNum :: [String] -> [Int]
toNum = map read 

bounds str = toNum $ splitOn "-" $ (!!) (words str) 0
ltr str = head $ take 1 $ (!!) (words str) 1
psswrd str = (!!) (words str) 2

-- example input = 1-7 q: qqqqxvqrkbqqztlqlzq
-- bnds, letter and password all are parsing the string input 
-- bnds -> [1,7]
-- letter -> 'q'
-- password -> "qqqqxvqrkbqqztlqlzq"

solveOne str = solve
  where bnds = toNum $ splitOn "-" $ (!!) (words str) 0
        letter = head $ take 1 $ (!!) (words str) 1
        password = (!!) (words str) 2
        count = length $ filter (==letter) password 
        solve = count >= (head bnds) && count <= (last bnds)

solveOneA str = solve
  where bnds = bounds str 
        letter = ltr str 
        password = psswrd str 
        amatch = (!!) password (head bnds -1) == (letter)
        bmatch = (!!) password (last bnds -1) == (letter)
        truthcount = length $ filter (\x -> x == True) [amatch, bmatch]
        solve = truthcount == 1
        
main = do 
  listof <- lines <$> readFile "adventofcode//day2data.txt"
  let passwordCount = length $ filter ( solveOne) listof
  print passwordCount
  return ()
