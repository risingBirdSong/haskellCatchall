import Debug.Trace
import Data.Bool
import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad
import Data.Ord 

class C a where
  m                     :: Show b => a -> b

-- class C a where m :: Show b => a -> b you can read as
-- "If you want to write an instance of the type class C for any type a, you have to provide an implementation of m :: Show b => a -> b"
-- You can do that by writing, for example

-- Here's an example type constructor
data Pair a = MkPair a a
-- Oh let me be less ambiguous
-- Again, Pair is a type constructor
-- MkPair is not
-- It's a constructor
-- Not a type constructor
-- Big difference!!!


-- i did get taught what you wrote

split' :: Eq a => a -> [a] -> [[a]]
split' c [] = []
split' c xs = xs' : if null xs'' then [] else split' c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs
-- split' c xs | trace ("showing" ++ show xs) False = undefined


taker c xs = takeWhile (/=c) xs
dropper c xs = dropWhile (/=c) xs

aaa = "codeleet"
bbb = [4,5,6,7,0,2,1,3]

shfl' :: (Ord a, Ord b) => [a] -> [b] -> [b]
shfl' = ((map snd . L.sort) . ). zip


type Square = [[Integer]]

square = [[1,2,3],[4,5,6],[7,8,9]]

pp :: Square -> IO ()
pp   = putStrLn . unlines . map (unwords . map show )

ptr = putStrLn "1 2 3\n"
unwrd str = map (unwords . map show) str
unlineda =  unlines ["aa","bb","cc","dd","ee"]
-- *Main> putStrLn unlineda
-- aa
-- bb
-- cc
-- dd
-- ee

nubby lst = map (fst) $ M.toList ( nubby' (lst) (M.fromList []))
nubby' [] mp = mp
nubby' (x:xs) mp 
  | M.lookup x mp == Nothing = nubby' (xs) (M.insert x x mp)
  | M.lookup x mp == Just x = nubby' (xs) (mp)

myMap = M.fromList [(1,1),(2,2),(3,3)]


-- maybe practice

maybeMap lst ctf = map (\ x -> if (x > ctf) then Just x else Nothing) lst
-- *Main> maybeMap [2,4,6,8,10] 4
-- [Nothing,Nothing,Just 6,Just 8,Just 10]


maybeadd :: Maybe Int -> Maybe Int
maybeadd (Just a) = Just (a+1)
maybeadd _ = Nothing


main :: IO ()
main = do   print (maybeadd(Just 4))
            print (maybeMap [1,2,3,4,5,6,7,8,9,10] 5)

-- maybeadd' :: Maybe Int -> Maybe Int
-- maybeadd' 
--   | (Just a) = Just (a+1)
--   | otherwise = Nothing

maybeadd' ma
  | Just a <- ma = Just (a+1)
  | otherwise    = Nothing

myvar :: [Maybe Char]
myvar = maybeMap ['a', 'g','z'] 'd'

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (L.transpose [xs, ys])

interleave' :: [a] -> [a] -> [a]
interleave' a b =  concat (f a b)  where  
  f [] _ = []
  f _ [] = []
  f (x:xs) (y:ys) = [x,y] : f (xs) (ys) 

hello :: String -> IO ()
hello x = putStrLn $ "hello " ++ x 

truthyFalsy ls 
  | length ls == 0 = False
  | otherwise = True

class TruthyFalsy a where 
    toBool :: a -> Bool

-- come back and implemente this later
-- instance truthyFalsy List where 
--     [] = False 

  

-- triplet :: (Ord a, Fractional a) => [a] -> [[a]]
triplet ns =  truthyFalsy $ head [[x,y,z] | x <-ns, y <- ns, z <- ns, let add = (x+y+z), 1.0 < add && add < 2.0 ] 

-- findIt limit (x:xs) = (x:) <$> findIt (limit -x) xs

findIt _ [] = []
findIt limit (x:xs)
    | (x < limit) = (x) : findIt (limit -x) xs
    | otherwise = findIt (limit) xs


findRang _ _ [] = []
findRang lwr upr (x:xs)
  | (x > lwr) && (x < upr) = x : findRang (lwr + x) (upr - x) xs
  | otherwise = findRang (lwr) (upr) xs

-- findRang 10 100 [1..100]
-- [11,22,44]

-- the way null is implemented on foldable is interesting / cool 
-- null' :: t a -> Bool
null' :: Foldable t => t a -> Bool
null' a = foldr (\_ _ -> False) True a


data Item a = One a | Many [Item a]

example1 = Many [One 1,One 2,Many [One 3,Many [One 4,Many [One 5,One 6,Many []]]]]
emptyExample = Many [Many [Many []]]

onesExample = Many [One 1, One 2, One 3]


flatten (One x) = [x]
flatten (Many xs) = concatMap flatten xs