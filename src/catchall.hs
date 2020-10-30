import Debug.Trace
import qualified Data.List as L
import qualified Data.Map as M
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

nubby lst = nubby' (lst) (M.fromList [])
nubby' [] mp = mp
nubby' (x:xs) mp 
  | M.lookup x mp == Nothing = nubby' (xs) (M.insert x x mp)
  | M.lookup x mp == Just x = nubby' (xs) (mp)

myMap = M.fromList [(1,1),(2,2),(3,3)]



  