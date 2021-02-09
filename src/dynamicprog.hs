naivefib 0 = 0
naivefib 1 = 1
naivefib n = naivefib (n-1) + naivefib (n -2)

linearFib n = let fib = 0 : 1 : zipWith (+) fib (tail fib) in fib !! n
linearFib' n = let fib = 0 : 1 : zipWith (+) fib (tail fib) in fib !! n

naiveLCS :: String -> String -> String
naiveLCS [] _ = []
naiveLCS _ [] = []
naiveLCS (x:xs) (y:ys)
  | x == y    = x : naiveLCS xs ys
  | otherwise = max (naiveLCS (x:xs) ys) (naiveLCS xs (y:ys))

dpLCS :: String -> String -> Int
dpLCS _ [] = 0
dpLCS a b =
  let nextRow ac prevRow =
        let diagonals = 0:prevRow
            lefts = 0:thisRow
            ups = prevRow
            maxes = zipWith max lefts ups
            thisRow = zipWith3 (\diag maxLeftUp bc ->
                                   if bc == ac then 1 + diag else maxLeftUp)
                                   diagonals maxes b
        in thisRow

      firstRow = map (\_ -> 0) b
      dpTable = firstRow:zipWith nextRow a dpTable

  in last (last dpTable)


-- *Main> naiveLCS "nematode knowledge" "empty bottle"
-- "tole"