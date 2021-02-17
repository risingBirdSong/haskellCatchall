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