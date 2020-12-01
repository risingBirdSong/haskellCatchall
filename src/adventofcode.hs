import System.IO
import Control.Monad

main = do 
    handle <- openFile "adventexpenses.txt" ReadMode
    contents <- hGetContents handle 
    let singlewords =  words contents 
        list = cnvrt singlewords
    print list 
    hClose handle 

cnvrt :: [String] -> [Int]
cnvrt = map read 