import Data.List.Split
a = splitOn "," "a,bb,ccc,ddd"
b = splitOn " " "1-7 q: qqqqxvqrkbqqztlqlzq"

str = "1-7 q: qqqqxvqrkbqqztlqlzq"

clean str =  words str
cleaned = clean "1-7 q: qqqqxvqrkbqqztlqlzq"

toNum :: [String] -> [Int]
toNum = map read 

bounds str = toNum $ splitOn "-" $ (!!) (clean str) 0
ltr str = head $ take 1 $ (!!) (clean str) 1

psswrd str = (!!) (clean str) 2

password str = psswrd str
flt letter str = filter (==letter) (password str )

solveOne str = do 
   let bnds = bounds str 
   let letter = ltr str 
   let password = psswrd str 
   let count = length $ filter (==letter) password 
   let solve = count >= (head bnds) && count <= (last bnds)
   print solve
   return ()

