naivefib 0 = 0
naivefib 1 = 1
naivefib n = naivefib (n-1) + naivefib (n -2)

naiveLCS :: String -> String -> String
naiveLCS [] _ = []
naiveLCS _ [] = []
naiveLCS (x:xs) (y:ys)
  | x == y    = x : naiveLCS xs ys
  | otherwise = max (naiveLCS (x:xs) ys) (naiveLCS xs (y:ys))

linearFib n = let fib = 0 : 1 : zipWith (+) fib (tail fib) in fib !! n

linearFib' n = let fib = 0 : 1 : zipWith (+) fib (tail fib) in fib !! n

-- *Main> naiveLCS "nematode knowledge" "empty bottle"
-- "tole"