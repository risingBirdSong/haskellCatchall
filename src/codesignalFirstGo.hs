-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
-- {-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe
import Data.Ord
import Data.Function
import qualified Data.Set as S
import qualified Data.Matrix as Mtx
import qualified Data.Map as M 
import qualified Data.List.GroupBy as Grp  
import qualified Data.List.Ordered as Ordd  
import Data.List.Split 
import Control.Arrow



-- import Data.Ord

firstDuplicateNestedlambdas xs = groupBy (\(x, v1) -> \(y,v2) -> x == y ) . sort $ zip xs [0..]  
firstDuplicateCollapsed xs = groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  

firstDuplicate xs = solve go 
  where go =  sortBy (\x y -> compare (snd $ grabSecond x) (snd $ grabSecond y)) . filter ((>1).length) . groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]  
        solve [] = -1
        -- solve ([(x,i), _] : _) = x
        solve (((x,i) : _ ) : _ ) = x
        grabSecond (x:y:zs) = y


firstDInnerLogic xs = sortBy (\x y -> compare (snd $ grabSecond x) (snd $ grabSecond y)) . filter ((>1).length) . groupBy (\(x, v1) (y,v2) -> x == y ) . sort $ zip xs [0..]

grabSecond (x:y:zs) = y



tupleMatching [(x,y)] = "oneTuple"
tupleMatching [(x,y),(xx,yy)] = "twoTuple"
-- tupleMatching [(x,y),(xx,yy), _] = "moreThanTwoTuples"
tupleMatching _ = "moreThanTwoTuples"

ex = [2, 1, 3, 5, 3, 2]
solverPattern (x:xs) = x 

-- getFirstFromTuples (([(x,i) : _]) : _) = x
getTupleInn (((x,i) : _ ) : _ ) = x

-- *Main> solverPattern [[(1,2),(3,4)],[(4,5),(6,7)]]
-- [(1,2),(3,4)]






firstNotRepeatingCharacter strs = finder strs options
   where options = concat $ filter ((==1).length) . group $ sort strs 
         finder [] cands = '_'
         finder (x:xs) cands 
              | x `elem` cands = x 
              | otherwise = finder xs cands 

firstNotRepeatingCharacter' :: String -> Char
firstNotRepeatingCharacter' [] = '_'
firstNotRepeatingCharacter' (x:xs) | x `elem` xs = firstNotRepeatingCharacter (filter (/=x) xs)
                                  | otherwise = x 

firstNotRepeatingCharacter'' s = go s s
    where go _ [] = '_'
          go s (x:xs)
                  | length (filter (==x) s) == 1 = x
                  | otherwise = go newLs newXs        
              where newLs = filter (/= x) s
                    newXs = filter (/= x) xs


firstNotRepeatingCharacter''' = go S.empty
    where go _ [] = '_'
          go repeats (x:xs) = 
            if (S.member x repeats) || (elem x xs)
                then go (S.insert x repeats) xs
                else x


options strs = concat $ filter ((==1).length) . group $ sort strs 

-- "abbbbcddffghhhhhiiijjkkkklnnnoopppqqrrsssttuvvxxxxyy"
-- "ngrhhqbhnsipkcoqjyviikvxbxyphsnjpdxkhtadltsuxbfbrkof"

-- an idea from melissa regaring the baf
-- fnrc xs = let bag = Map.fromListWith (+) . zip xs $ repeat 1 in find ((== 1) . (bag !)) xs



counting val cur = if val == cur then (Just (+1)) else Nothing 

  -- where myMap = M.fromList []
        -- go [] count = count 
        -- go (x:xs) count = M.updateWithKey (counting x) count 

countElems xs = M.fromListWith (+) $ zip xs (repeat 1)

countA xs = M.fromListWith (+) $ map (\x -> (x,1)) xs
countB zs = map (\xs -> (head xs, length xs)) . group . sort $ zs 

frstNonRepeat xs = fromMaybe '_' $ find (`elem`singles) xs
  where singles = map fst . filter ((==1).snd) $ countB xs
        countB zs = map (\xs -> (head xs, length xs)) . group . sort $ zs 


exM = [[1, 2, 3],
     [4, 5, 6],
     [7, 8, 9]]

cw = map reverse . transpose

centuryFromYear year = let (centry, decadeAndYear) =  year `divMod` 100 
                           answer (cntr, year) 
                              | year > 0 = cntr + 1
                              | otherwise = cntr
                       in answer (centry, decadeAndYear)

centuryFromYear' year = ceiling (year / 100)
centuryFromYear'' year = 1 + ((year -1 ) `div` 100 )

palindrome str = str == reverse str

adjacentElementsProduct xs = maximum $ go xs 
      where go [x] = []
            go (x:y:zs) = (x * y) : go (y:zs)
             
adjacentElementsProduct xs = maximum $ zipWith (*) xs (tail xs)  

shapeArea 1 = 1
shapeArea n = (n-1) * 4 + shapeArea (n-1)


makeArrayConsecutive2 s = (maximum (s) - minimum s + 1) - length s 


-- almostIncreasingSequence s = 
strctIncExA :: [Integer]
strctIncExA = [1, 3, 2, 1]
strctIncExB = [1, 3, 2]
srted = [0..10]

-- [1, 2, 1, 2]
-- [40, 50, 60, 10, 20, 30]


almstA sequence =
    case findIndex (not. okPair) (prs sequence) of
        Nothing -> True
        Just x -> any isOk. map (flip delete sequence) $ [x, x+1]
    where prs seq = zip seq (tail seq)
          delete x = (\(l1, l2) -> l1 ++ drop 1 l2). splitAt x
          isOk = all okPair. prs
          okPair = uncurry (<)


almostIncreasingSequence s = and $ (<2) . length 
                            . filter (uncurry (>=)) 
                            . zip s . tail <$> [s, tail s]


almst1A s = zip s . tail <$> [s, tail s]

myMatrix =[[0,1,1,2], 
      [0,5,0,0], 
      [2,0,3,3]]

ghosts xxs = sum $ concatMap (takeWhile (/= 0)) $ transpose xxs



almostDupes xs = if (length xs - length (nub xs) > 1) then False else True 
-- almostOrderA xs = ((<2).length) $ filter (==False) $ zipWith (<) xs (tail xs)\

almostOrderA xs = (<2) $ sum $ zipWith3 zipping xs (tail (xs ++ [maximum xs + 1])) (tail $ tail xs ++ [maximum xs + 1])

almostOrderATest xs = zipWith3 zipping xs (tail (xs ++ [maximum xs + 1])) (tail $ tail xs ++ [maximum xs + 1])

-- almostOrderA xs = zipWith3 zipping xs (tail (xs ++ [maximum xs + 1])) (tail $ tail xs ++ [maximum xs + 1])

zip3Test xs = zip3 xs (tail (xs ++ [maximum xs + 1])) (tail $ tail xs ++ [maximum xs + 1])


zipping a b c 
      | a >= b && a >= c = 2
      | a >= b || a >= c = 1 
      | otherwise = 0 

-- almostOrderB xs = ((<2).length) $ filter (==False) $ zipWith (<) xs (tail $ tail xs)
almostComposed xs = (almostDupes xs) && (almostOrderA xs)
almostOrderB xs = zip xs (tail xs)
almostOrderC xs = zip xs (tail $ tail xs)

-- takeTest = takeTwo ((==).length) ["ab", "cd","asdf"]

allLongestStrings xs = reverse $ takeWhile (\x -> length x == longest) $ reverse $ srtd
   where longest = length $ last srtd   
         srtd = sortOn length $ xs
-- assume that the dupe test comes first, and then a nubbed xs version is processed
ag1 = [1,2,3,2,4,5] -- True (1 dupe vltn) (0 order violations, bcz of nub) 
ag2 = [10,1,2,3,4,5] -- True (0 dupe vltn) (1 ord vltn)
ag3 = [10,1,2,4,5] -- True (0 dupe vltn) (1 order vltn)

ag4 = [3,4,5,3,4,5] -- False (3 dupe vltn) (0 order vltn)

ag5 = [1,1,1,3,4] -- False (2 dupe vltn) (0 order vltn)

ag6 = [9,6,7,2,8] -- False (0 dupe vltn) (2 order vltn)
ag7 = [6,7,8,3,4,5] -- False (0 dupe vltn) (3 order vltn)

-- almstAgain xs = (zipSrt)
--       where dupViolations = (length xs) - (length (nub xs))
--             zipSrt =  sort $ zip (nub xs) [0..] 

-- ("dupe violations -> ", show dupViolations , "order violations -> " ++ show grpVltns)

grpBy :: (a -> a -> Bool) -> [a] -> [[a]]
grpBy _ [] = []
grpBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])

almstAgainNub xs = solve dupViolations  ordVltns
      where dupViolations = (length xs) - (length (nub xs))
            strdidxs = map (snd) $ sort $ zip (nub xs) [0..]
            ordVltns = sum $ map (\x -> length x -1) 
                       $ transpose $ grpBy (<) $ map snd 
                       $ sort $ zip (nub xs) [0..]
            findFirstDupe xs = head $ head $ filter ((>1).length) $ group $ sort xs  
            solve dp or 
                  | (dp + or) < 2 = True
                  | dp == 1 && or == 1 = almstAgainNub (delete (findFirstDupe xs) xs)
                  | otherwise = False

almstAgainSet xs = solve dupViolations  ordVltns
      where dupViolations = (length xs) - (length (nubby xs))
            strdidxs = map (snd) $ sort $ zip (nubby xs) [0..]
            ordVltns = sum $ map (\x -> length x -1) 
                       $ transpose $ grpBy (<) $ map snd 
                       $ sort $ zip (nubby xs) [0..]
            findFirstDupe xs = head $ head $ filter ((>1).length) $ group $ sort xs  
            nubby xs = S.toList $ S.fromList xs
            solve dp or 
                  | (dp + or) < 2 = True
                  | dp == 1 && or == 1 = almstAgainSet (delete (findFirstDupe xs) xs)
                  | otherwise = False


myNubPreservesOrder xs = go xs (S.empty) 
      where go [] st = []   
            go (x:xs) st
                  | S.member x st = go xs st 
                  | otherwise  = x : go xs (S.insert x st)


failing1 = [1, 2, 5, 3, 5]

simpleLessThanCount xs = filter (==False ) $ zipWith (<) (xs) (tail xs)
      
sortTest xs = Grp.groupBy (<) $ map snd $ sort $ zip (nub xs) [0..]
sortTestTrnsp xs = transpose $ Grp.groupBy (<) $ map snd $ sort $ zip (nub xs) [0..]
sortTestViolationCount xs = sum $ map (\x -> length x -1) $ transpose $ Grp.groupBy (<) $ map snd $ sort $ zip (nub xs) [0..]
      

cmA = "aabcc" 
cmB = "adcaa"


s1 = "abca"
s2 ="xyzbac"

z1 = "zzzz"
z2 = "zzzzzzz"

commonCharacterCount s1 s2 = length $ concatMap minimum $ gather overlap s1 s2 
      where overlap = nub $ s1 `intersect` s2 
            gather [] s1 s2 = []  
            gather (x:xs) s1 s2 = [(filter (==x) s1), (filter (==x) s2)] : gather xs s1 s2  

commonCharacterCountTest s1 s2 = (sort s1, sort s2, s1 \\ s2, s2 \\ s1)
commonCharacterCount' s1 s2 = length lngst - (length $ lngst \\ shrt)
      where lngst = last $ srtd 
            shrt = head $ srtd
            srtd = sortOn length [s1,s2] 

intr s1 s2 = s1 `intersect` s2

-- best solutuion! this works in either direction, either with the shorterst word \\ or the longest word \\...
-- the original s minus the difference between the two strings... if the longer is divided, the longest word minus the longer difference will equal the shortest word minus the shorter distance
-- an example -> short = "zzzz" long "zzzzzzz" , long \\ short is "zzz" (length 3), so the orgiginal long, length 7 minus difference 3 is 4
-- the reverse, short \\ long is 0, all the short letters are subracted, but its original length is 4, so 4 - 0 is still 4 
-- so you can see the difference doesnt matter, and why this is the cleanest answer 
commonCharacterCount'' s1 s2 = length s1 - length (s1 \\ s2) 

-- mxLng s1 s2 = 

isLucky n = (\(x,y) -> (sum (numToNumList x)) == (sum (numToNumList y))) $ (\(a,b) ->((read a :: Int),(read b :: Int))) $ splitAt ((length toStr) `div` 2) toStr
      where toStr = show n

numToNumList n = reverse $ go n 
  where go 0 = []
        go num = snd (num `divMod` 10) : go (num `div` 10)