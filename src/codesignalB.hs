import Data.List
import Data.List.Split
import Data.Char
import Data.Function
import Data.Ord
buildPalindrome xs =  concat $ concatMap (nub) $ group $ (group xs) ++ (group $  reverse xs)
buildPalindrome' xs =  (group xs) ++ (group $  reverse xs)

buildPalindromenaive xs = go xs [] []
  where go (x:xs) not palis
          | (x:xs) == reverse (x:xs) = (not, xs:palis)
          | otherwise = go xs (x:not) palis 


isMAC48Address str = ((and $ concat [(concat validchars)]) && validlast && notrailing)
    where prepare =  split (condense $ oneOf "-") str
          chunked =  chunksOf 2 prepare
          firstbit = take 5 chunked
          charlogic = (`elem`  (['0'..'9'] ++ ['A'..'F']) )
          validchars = map ((map charlogic).head) firstbit 
          validlast = (all charlogic $ head $ last  chunked) 
          notrailing = not . null $ head $ last chunked

          checkhyphens = map ((=="-").last) firstbit
isMAC48Address' str =  concat $ splitOn "-" str

lineEncoding str = concatMap lgc $ group str
  where lgc [xs] = [xs]
        lgc s = (show $ length s) ++ [head s] 
          


board = concatMap (\n -> zipWith (\a b -> (fromEnum a) + (fromEnum b)) (['A'..'Z']) (repeat n)) [1..8] 


deleteDigit n = stichBackDeleted $ deleter $ numstoList n 
numstoList n = map digitToInt $ show n 
listtoint ns =  read(map (intToDigit) ns) :: Int
        
deleter ns = filter ((/= (length ns)).length) $ snd $ mapAccumL (\acc list -> (acc+1, delete acc list)) 0 (replicate (length ns + 1) ns)
    where zipped = zip [0..] ns

stichBackDeleted xss = maximum $ map (listtoint) xss


-- longestWord txt = maximumBy (comparing length) $ map (filter (isAlpha)) $ words txt


longestWord txt = maximumBy (comparing length) $ splitWhen (not.isAlpha) txt

handleTime "24" "00" = False 
handleTime h' m' = ( h >= 0 && h <= 24) && (m >= 0 && m <= 60) && (head h' /= '0')
      where h = (read h' :: Int)
            m = (read m' :: Int)
            
validTime time = (\[h,m] -> handleTime h m ) $ filter (not . null) $ splitWhen (not . isNumber) time