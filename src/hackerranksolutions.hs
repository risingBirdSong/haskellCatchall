import Data.List

-- Simple Array Sum

main = do
    n <- getLine
    ns <- map read . words <$> getLine
    print $ sum ns



lineToInts :: String -> [Int]
lineToInts line = map (\i -> read i :: Int) $ words line

sumDiagonal :: [[Int]] -> Int
sumDiagonal [] = 0
sumDiagonal (x:xs) = (head x) + sumDiagonal (map tail xs)

mainA :: IO ()
mainA = do
    getLine
    matrix <- fmap (map lineToInts . lines) getContents
    let leftRight = sumDiagonal matrix
    let rightLeft = (sumDiagonal . reverse) matrix
    (putStrLn . show . abs) (leftRight - rightLeft)