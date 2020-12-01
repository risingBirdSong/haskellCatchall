import System.IO

main = do 
    handle <- openFile "adventexpenses.txt" ReadMode
    contents <- hGetContents handle 
    let singlewords =  words contents 
        list = cnvrt singlewords
    let dd = [ x * y | x <- list, y <- list, x + y == 2020] 
    print $ head dd
    hClose handle 
cnvrt :: [String] -> [Int]
cnvrt = map read 