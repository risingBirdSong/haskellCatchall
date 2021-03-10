import Data.List
import Data.List.Split
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
          