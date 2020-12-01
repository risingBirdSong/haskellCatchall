import System.IO

main = do 
    handle <- openFile "adventexpenses.txt" ReadMode
    contents <- hGetContents handle 
    let singlewords =  words contents 
        list = cnvrt singlewords
    let dd = [ x * y * z | x <- list, y <- list, z <- list,  x + y + z == 2020] 
    print $ head dd
    hClose handle 
cnvrt :: [String] -> [Int]
cnvrt = map read 