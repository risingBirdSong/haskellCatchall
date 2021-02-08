numToNumList n = reverse $ go n 
  where go 0 = []
        go num = snd (num `divMod` 10) : go (num `div` 10)

toNum lst = fst $ foldr (\x (acc,place) -> (acc + (place * x) , place * 10) ) (0,1) lst
toListNums strnum = map (\x -> read [x] :: Int ) $ drop 1 $ init $ show strnum 

filterTwo f [] = [] 
filterTwo f [x] = [] 
filterTwo f (x:y:ls)
  | (f x y) = x : y : filterTwo f (y:ls)
  | otherwise = filterTwo f (y:ls)



-- a helper function I wrote wanting to zip when the values are Just, otherwise stop zipping
maybeZip ((Just x):xs) ((Just y):ys) = (Just x, Just y) : maybeZip xs ys
maybeZip _  _ = [] 