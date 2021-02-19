-- https://app.codesignal.com/interview-practice/task/pMvymcahZ8dY4g75q/description
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Char
import Data.List.Split
-- import Data.Maybe
import Data.Ord
import Data.Function
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Matrix as Mtx
import qualified Data.Map as M 
import qualified Data.List.GroupBy as Grp  
import qualified Data.List.Ordered as Ordd  
import Data.Maybe
import Data.Tuple
import Data.List.Split 
import Control.Arrow
import qualified Data.Array as A 
import qualified Data.Vector as V
import qualified Data.Sequence as Sq 







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

failFast = [1,2,3,2,5,3,4,1,5,3,2,4,1]

understandingalmostIncreasingA s = zip s . tail <$> [s, tail s] 
understandingalmostIncreasingB s = filter (uncurry (>=)) . zip s . tail <$> [s, tail s] 
understandingalmostIncreasingC s = (<2) . length . filter (uncurry (>=)) . zip s . tail <$> [s, tail s] 
understandingalmostIncreasing s = and $ (<2) . length . filter (uncurry (>=)) . zip s . tail <$> [s, tail s] 


almostIncreasingSequenceInsprd xs = go (trio xs) 0 
      where go [] bad = trace ("bad ->" ++ show bad)  True
            go ((a,b,c):xs) bad 
                  | bad > 1 = False
                  | a >= c && b >= c = False
                  | a >= b = go (drop 1 xs) (bad + 1)
                  | a < b = go (drop 1 xs) (bad)
            trio xs = zip3 (xs) (tail xs) (tail $ tail xs)

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

threeZip xs = zip3 (xs) (tail xs) (tail $ tail xs)

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


sortByHeight' xs = putBack xs srtPeople
    where srtPeople = sort $ filter (/=(-1)) xs
          putBack [] _ = []
          putBack xs [] = xs
          putBack (x:xs) (p:ppl)
            | x == (-1) = x : putBack xs (p:ppl)
            | otherwise = p : putBack xs ppl

srtPeople xs = sort $ filter (/=(-1)) xs
s xs = sort $ filter (/=(-1)) xs

sortOnly p xs = reinsert xs $ sort $ filter p xs
  where
    reinsert _ [] = []
    reinsert (-1:xs) ys = -1 : reinsert xs ys
    reinsert (_:xs) (y:ys) = y : reinsert xs ys

sortExceptTrees = sortOnly (>0)

data Rev a = One a | Lst [Rev a] deriving (Show, Eq, Ord) 

myRev (Lst xss) =  go xss 
      where go [] = []
            go ((One x):xs) =  (One x) : go xs 
            go ((Lst aas):xs) =  Lst (( myRev (Lst (reverse aas)))) : go xs  
-- the current problem is it's not reversing the nested elements properly 
-- [One 'b',One 'a',One 'r',Lst [One 'z',One 'a',One 'b']]

alternatingSums zs = go zs [] []
      where go [] aa bb = [sum aa, sum bb]
            go (x:y:zs) aa bb = go (zs) (x:aa) (y:bb)
            go (x:zs) aa bb = go [] (x:aa) bb



-- "foo(bar)baz"
-- reverseInParentheses xs = rever $ go   
--       | s == '(' = 

pic1 =  ["abc",
           "ded"]

pic2 = ["aa", 
 "**", 
 "zz"]

addBorder pct = brdr : (map (\ln -> '*' : ln ++ "*") pct) ++ [brdr]
            where width = length $ head pct 
                  brdr = replicate (width + 2) '*'

sortplay as bs = null bads
      where discardGoods =  filter (\(x,y) -> x /= y) $ zip as bs 
            compareOrder = zip discardGoods (tail discardGoods)
            bads =  filter (\(x,y) -> x /= (swap y)) compareOrder


howMany xs = go (head xs) (drop 1 xs) (0)
      where go high [] count = count 
            go high (x:xs) count 
                  | high >= x = go (x + needGrow) (xs) (count + needGrow) 
                  | otherwise = go (x) (xs) (count)
              where needGrow = high - x + 1


makePali xs = 2 > (length $ filter (odd) $ map length $ group $ sort xs)


areEquallyStrong yl yr fl fr = (yl + yr == fl + fr) && ( (length allarms) > (length $ nub allarms))
    where allarms = [yl, yr, fl, fr]


arrayMaximalAdjacentDifference xs = maximum . map abs $ zipWith (-) xs (tail xs)


avoidObstacles obst = safeStep prepare obst (maximum obst + 1) (maximum prepare)
      where prepare = delete 0 (sort $ take (maximum obst )[0..] \\ obst)
            debugging = ((prepare) , obst , maximum obst + 1 , (maximum prepare))


-- safeStep :: (Ord a, Num a) => [a] -> a -> a
safeStep xs obstacles finalGoal lastStep = solve reject
      where mainLogic = filter (\sublist -> lastStep `elem` sublist ) 
                       $ map (\x -> takeWhile (`elem` xs) (iterate (+x) x)) (delete 0 xs)
            prepareForSolve = concat $ mainLogic
            iterated = map (\x -> takeWhile (<finalGoal) (iterate (+x) x)) (delete 0 xs)
            reject = filter (\ys -> null $ (intersect ys obstacles)) iterated   
            solve cands 
                  | null cands = finalGoal
                  | otherwise = minimum $ map head cands

obst1 =  [5, 3, 6, 7, 9]


avoidObstacles' xs = minimum $ filter (isSafeStep xs)  steps
      where steps = [1..(maximum xs + 1)]
isSafeStep obst cand = not $ any (\o -> o `mod` cand == 0 ) obst


-- https://app.codesignal.com/arcade/intro/level-5/5xPitc3yT3dqS7XkP


-- boxBlur image = sum $ concat image


imageMtrx xss = Mtx.fromLists xss 

mtxA :: (Num a) => Mtx.Matrix a
mtxA = Mtx.submatrix 1 3 1 3 $ imageMtrx  image1
mtxB = Mtx.submatrix 1 3 2 4 $ imageMtrx  image1
mtxC = Mtx.submatrix 2 4 1 3 $ imageMtrx  image1
mtxD = Mtx.submatrix 2 4 2 4 $ imageMtrx  image1
-- mtxBad = Mtx.submatrix 0 2 2 4 $ imageMtrx  image1

goal1 = [13,13,24,24]
goal2 = [13,24,13,24]

makeCoords lngth = take lngth $ zip [1..] (tail [2..] )
batches tups amount = concat $ map (replicate amount) tups
stitched tups amount = concat $ take amount $ repeat tups

batchesAndStiches amount = take (amount * 2) $ zip (batches (makeCoords 2) amount) (stitched (makeCoords 2) amount) 
reshape tups = map (\((a,b), (c,d)) -> (a,b,c,d)) tups

--  1 3 1 3  
--  1 3 2 4  
--  2 4 1 3 
--  2 4 2 4 


-- zipping xs ys = 

-- 
-- proof of concept! \/ !
-- *Main> mtxSummer mtxD `div` 9
-- 4
-- *Main> mtxSummer mtxA `div` 9
-- 5
-- *Main> mtxSummer mtxB `div` 9
-- 4
-- *Main> mtxSummer mtxC `div` 9
-- 4
-- *Main> mtxSummer mtxD `div` 9
tryBox xss = chunksOf halfLength $ map (`div` 9) $ map (\(rs,re,cs,ce) -> sum $ Mtx.submatrix rs re cs ce toMtx) coordinates
      where toMtx = Mtx.fromLists xss 
            coordinates =  reshape (batchesAndStiches halfLength)
            halfLength = length xss `div` 2

subGenToCoor xs amount = map (\(x,y) -> take amount $ repeat [x,y] ) xs

coordGenerateAAA = filter ((==2).length)$ nub $ subsequences [[1,3],[1,3],[2,4],[2,4]]
coordGenerateBBB xss = filter ((==2).length)$ nub $ subsequences (xss ++ (reverse xss))
-- [[1,3],[1,3],[2,4],[2,4]]


simpleEx = [1,2,3,4]

image = [[1, 1, 1], 
         [1, 7, 1], 
         [1, 1, 1]]
image1 :: (Num a) => [[a]]
image1 = [[7, 4, 0, 1], 
         [5, 6, 2, 2], 
         [6, 10, 7, 8], 
         [1, 4, 2, 0]]

image2 = [[36,0,18,9], 
         [27,54,9,0], 
         [81,63,72,45]]

-- [[36,0,18],[0,18,9],[27,54,9],[54,9,0],[81,63,72],[63,72,45]]

image3 = [[36,0,18,9,9,45,27], 
 [27,0,54,9,0,63,90], 
 [81,63,72,45,18,27,0], 
 [0,0,9,81,27,18,45], 
 [45,45,27,27,90,81,72], 
 [45,18,9,0,9,18,45], 
 [27,81,36,63,63,72,81]]

--  [[36,0,18],[0,18,9],[18,9,9],[9,9,45],[9,45,27],[27,0,54],[0,54,9],[54,9,0],[9,0,63],[0,63,90],[81,63,72],[63,72,45],[72,45,18],[45,18,27],[18,27,0],[0,0,9],[0,9,81],[9,81,27],[81,27,18],[27,18,45],[45,45,27],[45,27,27],[27,27,90],[27,90,81],[90,81,72],[45,18,9],[18,9,0],[9,0,9],[0,9,18],[9,18,45],[27,81,36],[81,36,63],[36,63,63],[63,63,72],[63,72,81]]


overlap xs =  filter ((==3).length) $ go xs 
      where go [] = [] 
            go xs = take 3 xs :   go (drop 1 xs)
overlapped xss =  map (map overlap) $ overlap  xss  
subGrids xxs =  map transpose $ overlapped xxs
solver xxs = map (map ((`div`9).sum.concat)) $ subGrids xxs
-- start again 

-- getThree (x:y:z:[]) = [x,y,z] 
getThree (x:y:z:rst) = x:y:z : getThree (y:z:rst) 
getThree _ = []

gettingthree xss = map getThree (getThree xss)

boxBlur' xss = transpose $ map (map ((`div`9).sum.concat)) $ map (chunksOf 3) $ transpose $ map (chunksOf 3) $ gettingthree xss


minesweepprint mtrx = mapM_ (\x -> print x) mtrx

minesweeper =[[True,True,True], 
            [True,True,True], 
            [True,True,True]]

-- sweeping mybmtrx = go mybmtrx 
--       where go (x:y:z:rst)
--             go (_) = []  
            

-- mynumsA =  boolstonums mymtrxr
mymtrxrB = [[True,False,False], 
            [False,True,False], 
            [False,False,False]]
mynumsB = boolstonums mymtrxrB


      
mtrxAdd (a:b:xs) = (a+b) : mtrxAdd (b:xs) 
mtrxAdd (_) = []

ddd =[[True,False,False,True], 
 [False,False,True,False], 
 [True,True,False,True]]

minesweeperSolutionAAA x = (zipWith.zipWith) (\a b -> a - fromEnum b) newX x 
  where 
    newX = sumOf3 $ sumOf3 x'
    sumOf3 = transpose . map genAAA
    x' = (map.map) fromEnum x

minesweeperSolutionAAATest x = (newX, x) 
  where 
    newX = sumOf3 $ sumOf3 x'
    sumOf3 = transpose . map genAAA
    x' = (map.map) fromEnum x


genAAA ls = zipWith3 (\a b c -> a + b + c) (0 : ls) ls (tail ls ++ [0])


sum3 x y z = x + y + z
map2n = map . map
zip2n = zipWith . zipWith

sumRow a = zipWith3 sum3 a (0:a) (tail a ++ [0])
countAll = transpose . map sumRow . transpose . map sumRow

minesweeperBBB a = zip2n (-) (countAll an) an
    where an = map2n fromEnum a




-- single row will zip three locations together
-- prepending 0 and appending 0 to our list will give us some padding on the left and right 
-- so that we can safely add three values without worrying about out the edges
-- this put the bomb count on the neighboring tiles... not, the original bomb numbers
-- will still be there and will need to be cleaned up later by -> [new list] - [original list]
-- example  -> 
      -- [1,0,0,1] (original list)
      -- becomes 
      -- [1,1,1,1] (new list)
      -- which will need to be cleaned to [0,1,1,0] because a bomb doesnt count distance to itself
      -- only adjacent cells count to disctance to their neighbor bombs

-- note, a good thing about singleRow function, using prepending + appending with zipwith3 is that
-- it cleans up after itself, it doesnt leave the padding after it calculates, meaning the input is the 
-- same length as the output

boolNum x 
      | x = 1 
      | otherwise  = 0

-- boolstonums :: Num a => [[Bool]] -> [[a]]
boolstonums xs = (map (map (boolNum)) xs)
singlerow row = zipWith3 (\a b c -> a + b + c) (0:row) (row) (tail row ++ [0])
myminesweep xxs = (zipped)
      where converToNums = map (map fromEnum) xxs 
            horzVertical =  transpose . map singlerow . transpose . map singlerow $ converToNums
            zipped = zipWith (zipWith (-)) horzVertical converToNums 

zzz = [[True,False,False,True], 
      [False,False,True,False], 
      [True,True,False,True]]

-- [[1,0,0,1], 
-- [0,0,1,0],
-- [1,1,0,1]]

-- [[0,2,2,1], 
--  [3,4,3,3], 
--  [1,2,3,1]]

-- doubleMap singlerow converToNums

ipstr = "172.16.254.1"

isIPv4Address s = length l == 4 && all valid l
  where l = splitOn "." s
        valid t = not (null t) && all isDigit t && read t <= 255


arrayReplace arr rplc subst = map (\x -> if x == rplc then subst else x) arr


-- i was confused what this is doing, (:[]) , I learned that (:[]) is "lifting" the char into a [char]
-- because that's what read expects, to be able to convert to a number
-- map (:[]) [1,2,3]
-- [[1],[2],[3]]

-- #conversion 

digitsA :: Integer -> [Int]
digitsA = map digitToInt . show

digits :: Integer -> [Integer]
digits = map (read . (:[])) . show

evenDigitsOnly n = all even $ digits n

evenDigitsOnly' n = all (even . read . c2s) $ show n
  where
    c2s c = [c]

evenDigitsOnly'' n = all (\x -> even (read [x] :: Int)) (show n)

variableName n = not (null n) && logic && ( not $ isNumber (head n))
      where logic = all (\x -> isAlphaNum x || x == '_') n

-- import Text.Regex.Posix
--variableName name = (name =~ "^[a-zA-Z]+[a-zA-Z0-9_]*$")

alphabeticShift str = map (\x -> toEnum (97 + ( (succ $ (fromEnum x) - 97) `mod` 26)) :: Char  ) str 

alphabeticShift' = map succ'
    where
        succ' 'z' = 'a'
        succ' x = succ x

alphabeticShift'' = map (toNext) 
    where toNext c = if c == 'z' then 'a' else succ c

sameColors a b 
      | even (a + b) = "dark"
      | otherwise  = "light"

strPosistion chr = find (\ (ltr,num) -> ltr == chr) $ zip (['A'..'H']) [1..8]   

singleChess piece = sameColors (snd $ fromJust (strPosistion (head piece))) ((\x -> read [x] :: Int) $ last piece)

chessBoardCellColor a b = singleChess a == singleChess b

chessBoardCellColor' cell1 cell2 = f cell1 == f cell2
    where
        f = odd . sum . map fromEnum

chessBoardCellColor'' (c1:cell1) (c2:cell2) = odd c1' == odd c2'
    where
        c1' = fromEnum c1 + read cell1
        c2' = fromEnum c2 + read cell2

circleOfNumbers n firstNumber = ((n `div` 2) + firstNumber) `mod` n   

depositProfit deposit rate threshold = go (toRational deposit) 0
      where go dps cnt  
              | dps >= (toRational threshold) = cnt 
              | otherwise = trace (show dps) go ((toRational ((toRational rate) / 100) * dps) + dps ) (succ cnt)  

-- def depositProfit(deposit, rate, threshold):
--     return math.ceil(math.log(threshold/deposit, 1+rate/100))

-- depositProfit deposit rate threshold = ceiling $ b `logBase` a
--   where a = fromIntegral threshold / fromIntegral deposit
--         b = 1.0 + fromIntegral rate / 100.0

tsda = [0..20]

absoluteValuesSumMinimization ns = snd.head $ sort $ map (absoluteWork ns) ns
absoluteValuesSumMinimizationSee ns = map (absoluteWork ns) ns
-- [(210,0),(191,1),(174,2),(159,3),(146,4),(135,5),(126,6),(119,7),(114,8),(111,9),(110,10),
-- (111,11),(114,12),(119,13),(126,14),(135,15),(146,16),(159,17),(174,18),(191,19),(210,20)]
absoluteWork arr n = (sum (map (\x -> abs(n - x)) arr), n)

absoluteValuesSumMinimization' a = a !! (div (length a - 1) 2)


-- ["ab", "bb", "aa"]


inputArray =["aba", 
      "bbb", 
      "bab"]

sortByEnum xs = foldr (+) 0  $ map fromEnum xs

oneDifference xs = all (\(x,y) -> length  (x \\ y) == 1 ) $ zip xs (tail xs)

stringsRearrangement xs = map (snd) $ sort $ map ((\x -> (sortByEnum x,x))) xs 


extractEachKth inputArray k =  map snd $ filter (\(i,v) -> i `mod` k /= 0) $ zip [1..] inputArray 

extractEachKth' [] k = [];
extractEachKth' a k = take (k-1) a ++ extractEachKth (drop k a) k

extractEachKth'' arr k = let
    (seg, rest) = splitAt (k - 1) arr
    in seg ++ case rest of
        [] -> []
        otherwise -> extractEachKth (tail rest) k


firstDigit str = fromMaybe '0' $ find (isNumber) str 

firstDigit' = head . filter isDigit 

differentSymbolsNaive st = length . group $ sort st

fgh = [2, 3, 5, 1, 6]
arrayMaxConsecutiveSum' xs k = maximum . map sum $ go xs
      where go [] = []
            go xs = take k xs : go (drop 1 xs)

arrayMaxConsecutiveSum'' xs k = go xs 0
      where go [] maxrng = maxrng
            go (xs) maxrng
              | (sum (take k xs)) > maxrng = go (drop 1 xs) (sum $ take k xs)
              | otherwise = go (drop 1 xs) maxrng

bgdata = [768, 77, 755, 960, 747, 25, 107, 520, 995, 404, 43, 714, 632, 642, 493, 352, 450, 625, 262, 217, 254, 55, 661, 822, 562, 187, 603, 216, 275, 76, 75, 417, 350, 942, 1000, 232, 887, 173, 858, 116, 75, 170, 529, 26, 62, 378, 667, 444, 240, 325, 444, 391, 698, 282, 870, 611, 974, 388, 586, 616, 845, 591, 525, 976, 938, 673, 413, 862, 396, 856, 764, 415, 309, 27, 583, 630, 741, 988, 456, 807, 242, 624, 149, 524, 962, 960, 900, 199, 645, 36, 343, 943, 232, 781, 445, 670, 177, 889, 57, 519]

arrayMaxConsecutiveSum''' ns k = go ns (V.fromList (replicate k 0)) 0 
      where go [] myVctr mxm = mxm  
            go (x:xs) myVctr mxm
                  | V.sum (myVctr) > mxm = go xs (V.snoc ((V.tail) myVctr) x ) (V.sum (myVctr))
                  | otherwise = go xs (V.snoc ((V.tail) myVctr) x ) mxm

mysq = Sq.fromList [1,2,3]
mysqa = (Sq.<|) 9 mysq


-- arrayMaxConsecutiveSum list n = maximum $ zipWith (-) nAhead runningTotal
--     where
--         runningTotal = scanl (+) 0 list
--         nAhead       = drop n runningTotal

arrayMaxConsecutiveSum list n = zip nAhead runningTotal
    where
        runningTotal = scanl (+) 0 list
        nAhead       = drop n runningTotal


-- growingPlant upSpeed downSpeed desiredHeight = desiredHeight `div` (upSpeed - downSpeed)

growingPlant upSpeed downSpeed desiredHeight = go upSpeed 
      where go height 
              | height >= desiredHeight = 1
              | otherwise = 1 + go ( height + (upSpeed - downSpeed)) 

-- foldr (\(v,w) acc -> v + acc ) 0 .
knapsackLight value1 weight1 value2 weight2 maxW = foldr (\(v,w) acc -> v + acc ) 0 
                                                   . takeWhile (\(w,v) -> v <= maxW) 
                                                   . scanl (\(_,accW) (v,w)-> (v, accW + w)) (0,0) 
                                                   . sortOn (Down) $ filter (\(v,w) -> w <= maxW) 
                                                    [(value1, weight1),(value2, weight2)]


longestDigitsPrefix = takeWhile isDigit

digitDegree n 
      | n < 10 = 0 
      | otherwise = 1 + digitDegree (adddigs n)  

adddigs :: Int -> Int 
adddigs n =  sum $ map (\x -> read [x]) $ show n 



digitDegree' :: Int -> Int
digitDegree' n = length $ takeWhile (>9) $ iterate f n where
    f = sum . map digitToInt . show

digitDegree'' n
    | n < 10 = 0
    | otherwise = digitDegree'' (sum $ digits' n) + 1

digits' = map digitToInt . show

-- bishopAndPawn bishop pawn = 

-- with list comprehensions, remember the fast moving / inner loop is the on the right hand side, like nm below...
diagmoves = chunksOf 8 [ (ltr,nm) |  nm <- [1..8] , ltr <-['a'..'g']] 
-- bottom to top, fast rows
trnsp1 = transpose diagmoves
-- bottom to top , fast cols 

diag = zip diagmoves (tail diagmoves)


prtPrint mtr = map (\x -> trace  "" x ) mtr

-- map (\(a,b) -> zip a (tail b) )
zipping'' = zip diagmoves diagmoves

-- *Main> prtPrint diagmoves
-- [
-- [('a',1),('b',1),('c',1),('d',1),('e',1),('f',1),('g',1),('h',1)],
-- [('a',2),('b',2),('c',2),('d',2),('e',2),('f',2),('g',2),('h',2)],
-- [('a',3),('b',3),('c',3),('d',3),('e',3),('f',3),('g',3),('h',3)],
-- [('a',4),('b',4),('c',4),('d',4),('e',4),('f',4),('g',4),('h',4)],
-- [('a',5),('b',5),('c',5),('d',5),('e',5),('f',5),('g',5),('h',5)],
-- [('a',6),('b',6),('c',6),('d',6),('e',6),('f',6),('g',6),('h',6)],
-- [('a',7),('b',7),('c',7),('d',7),('e',7),('f',7),('g',7),('h',7)],
-- [('a',8),('b',8),('c',8),('d',8),('e',8),('f',8),('g',8),('h',8)]]

ply = take 5 $ iterate (drop 1) [1..5]
-- [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5]]
ply1 = take 3 $ iterate (\x -> drop (head x) x) [2..10]
-- [[2,3,4,5,6,7,8,9,10],[4,5,6,7,8,9,10],[8,9,10]]

ply2 = take 5 $ iterate (\ x -> x ++ x) [4]
-- [[4],[4,4],[4,4,4,4],[4,4,4,4,4,4,4,4],[4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4]]

ply3 = take 5 $ iterate (drop 1.reverse) [1..5]
-- [[1,2,3,4,5],[4,3,2,1],[2,3,4],[3,2],[3]]

ply4 = take 4 $ iterate (intersperse '!') ['a'..'g']

ply5 = take 4 $ iterate (map (+1)) [1..4]
ply6 = take 4 $ iterate (tail) ply5

ply7 = take 4 $ iterate (\x -> drop (head x) x) [2..10]


stpA = map tails diagmoves
wantoCutDiagonal = [
      [[('a',1),('b',1),('c',1),('d',1)],[('b',1),('c',1),('d',1)],[('c',1),('d',1)],[('d',1)],[]],
      [[('a',2),('b',2),('c',2),('d',2)],[('b',2),('c',2),('d',2)],[('c',2),('d',2)],[('d',2)],[]],
      [[('a',3),('b',3),('c',3),('d',3)],[('b',3),('c',3),('d',3)],[('c',3),('d',3)],[('d',3)],[]],
      [[('a',4),('b',4),('c',4),('d',4)],[('b',4),('c',4),('d',4)],[('c',4),('d',4)],[('d',4)],[]]]

expA = zip (replicate 5 [1..5]) [0..5] 
expB = map (\(lst,amnt) -> drop amnt lst) expA

cutA = zip (stpA) [0..(length stpA)]
cutB = map (\(dta, drp) -> drop drp dta) cutA


getdiag x = zipWith (!!) x [0..]

dropMoreAndMore = take 10 $ scanl (flip drop) ['a'..'m'] [1..]
anotheroption = take 10 . map snd $ iterate (\(i, xs) -> (i + 1, drop i xs)) (1, ['a'..'m'])



-- import Control.Comonad
-- import Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
-- movingAverage n = extend ((/ fromIntegral n) . sum . NonEmpty.take n)
-- initial = (0 :| [0,0,0,0,0,0,0,100,0,0,0,0,0,0,0,0]) :: NonEmpty Float
-- mapM_ (print . NonEmpty.toList) . take 5 . iterate (movingAverage 2) $ initial

-- [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-- [0.0,0.0,0.0,0.0,0.0,0.0,0.0,50.0,50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-- [0.0,0.0,0.0,0.0,0.0,0.0,25.0,50.0,25.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-- [0.0,0.0,0.0,0.0,0.0,12.5,37.5,37.5,12.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-- [0.0,0.0,0.0,0.0,6.25,25.0,37.5,25.0,6.25,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

bishopAndPawn [x1, y1] [x2, y2] =
    let x = abs (fromEnum x1 - fromEnum x2)
        y = abs (fromEnum y1 - fromEnum y2)
    in x == y

enmLtrNum x y = uncurry (-) (fromEnum x, fromEnum y)


-- isBeautifulString str = 