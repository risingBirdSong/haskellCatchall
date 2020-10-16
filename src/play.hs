import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
-- reverse without built in 
split1 xs = chunksOf 1 xs
c = split1 "abcdefg"
-- rev' x = [x]
-- rev' (x:xs) =  (rev' xs) <> ([x])   

purify :: Int -> [Int]
purify x = pure x

-- rev' 

-- ah, useful example of id, the recursive condition can result in an empty array and that composes well with purify
-- [] <> (purify 3) <> (purify 4)

-- ah interesting, it works with just calling pure on x, I didn't think this would work at first
-- because when I run pure in the console on a num, I don't get [num], I just get num, which i didn't think 
-- would append without that structure, but clearly it does work like that.
-- rev' :: [a] -> [a]
rev' [] = []
rev' (x:xs) = (rev' xs) <> (pure x)

-- * Ambiguous type variable `f0' arising from a use of `print'
-- prevents the constraint `(Show (f0 Integer))' from being solved.
-- Probable fix: use a type annotation to specify what `f0' should be.
pured :: (Num a) => (Semigroup (f a), Applicative f) => a -> a -> a -> a -> f a
pured w x y z = (pure w)  <> (pure x) <> (pure y) <> (pure z) 

purednum :: [Int]
purednum = (pure 1)  <> (pure 2) <> (pure 3) <> (pure 4) 

-- I have this simple reverse function
-- ```
-- rev' [] = []
-- rev' (x:xs) = (rev' xs) <> (pure x)
-- ```
-- I was surprised when it worked, specifically pure here. 
-- When i've been doing little pure tests in the console, the only way it gets a structure is when I specifically give it a structure

-- examples
-- ```
-- pure 5 -> 5
-- ```
-- but giving it a specific type structure
-- ```
-- pure 5 :: [Int] -> [5]
-- ```
-- similarly ->
-- ```
-- pured = (pure 1) <> (pure 2) <> (pure 3) :: [Int]
-- ``` 
-- only works when I give it that type, otherwise error

-- So I'm curious how is rev' working properly with <> and pure without an explicit type signature?

data ListOf x = Nil | Cons x (ListOf x) deriving (Show, Eq, Ord)

myList = Cons 1 (Cons 2 (Cons 3 Nil)) -- Cons 1 (Cons 2 (Cons 3 Nil))

sum' Nil = 0
sum' (Cons num lst) = num + sum' lst 

insertInto val Nil = (Cons val Nil) 
insertInto val (Cons x list) = Cons x (insertInto val list) 

newList = insertInto 4 myList  -- Cons 1 (Cons 2 (Cons 3 (Cons 99 Nil)))
sumNewList = sum' newList -- 105

anotherNewList = insertInto 5 newList -- Cons 1 (Cons 2 (Cons 3 (Cons 99 (Cons 777 Nil))))

add x y = x + y

(reduce f x) Nil = x
(reduce f x) (Cons v l) = f v ((reduce f x ) l)

rdcTest = (reduce add 0) (anotherNewList) -- 15
rdc f acc list = (reduce f acc) (list) 
-- rdcd_a = add 0 anotherNewList
rdcTesta = rdc add 0 anotherNewList -- 15

productTest = rdc (\x y -> x * y) 1 anotherNewList -- 120

or' x y = x || y

boolLista = foldr insertInto Nil [True, True, False, True] -- Cons True (Cons False (Cons True (Cons True Nil)))
boolListb = foldr insertInto Nil [True, True, True, True] -- Cons True (Cons False (Cons True (Cons True Nil)))
boolListc = foldr insertInto Nil [False, False, False, False] -- Cons True (Cons False (Cons True (Cons True Nil)))


rdc_or_a = rdc (\x y -> x || y) False boolLista -- True
rdc_or_b = rdc (\x y -> x || y) False boolListb -- True
rdc_or_c = rdc (\x y -> x || y) False boolListc -- False

-- note there are equivalents built in, like
-- or [True, False, True] -> True

rdc_all_a = rdc (\x y -> x && y) True boolLista -- False
rdc_all_b = rdc (\x y -> x && y) True boolListb -- True

-- note there are equivalents built in, like
-- and [True, False, True] -> False
-- and [True, True, True] -> True


-- data ListOf x = Nil | Cons x (ListOf x) deriving (Show, Eq, Ord)
-- myList = Cons 1 (Cons 2 (Cons 3 Nil)) -- Cons 1 (Cons 2 (Cons 3 Nil))

aList = foldr insertInto Nil (reverse [1,2,3]) -- Cons 1 (Cons 2 (Cons 3 Nil))
bList = foldr insertInto Nil (reverse [4,5,6]) -- Cons 4 (Cons 5 (Cons 6 Nil))

-- (reduce f x) Nil = x
-- (reduce f x) (Cons v l) = f v ((reduce f x ) l)

copyList = reduce Cons Nil aList -- direct copy
appendList = reduce Cons bList aList -- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))

append' x y = reduce Cons x y 

uniqueOrder [] = []
uniqueOrder [x] = [x]
uniqueOrder (x:y:ls)
  | x == y = uniqueOrder (y:ls)
  | otherwise = x : uniqueOrder (y:ls)
  
doubleall = reduce doubleandcons Nil
  where doubleandcons num list = Cons (2*num) list

dble_ num = Cons (2*num) 
dlblerdx = reduce dble_ Nil
dbledList = dlblerdx aList --Cons 2 (Cons 4 (Cons 6 Nil))

duuble :: Num x => ListOf x -> ListOf x
duuble lst = reduce (\x -> Cons (x*2)) Nil lst 

-- duubleWrong lst = reduce (Cons (\x->2*x)) Nil lst
-- Expected type: ListOf (a -> a) -> ListOf x -> ListOf x
-- Actual type: ListOf (a -> a) -> ListOf (a -> a)

doubleandcons = fandcons double
  where double n = 2*n
fandcons f el list = Cons (f el) list

testdoubleandcons = reduce doubleandcons Nil aList -- Cons 2 (Cons 4 (Cons 6 Nil))

slickDble = reduce (Cons . (\x->2*x)) Nil 

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder [] = []
uniqueInOrder (f:l) = machine f l
  where 
    machine lastvalue [] = [lastvalue]
    machine lastvalue (x:xs) = 
      if x == lastvalue then
        machine x xs
      else lastvalue : machine x xs
    

-- uniqueInOrder :: Eq a => [a] -> [a]
-- uniqueInOrder [] = []
-- uniqueInOrder (x:y) = machine x y
--   where 
--     machine lastvalue [] = []
--     machine lastvalue (x:y) = 
--       if x == lastvalue then
--         machine x y
--       else x : machine x y

rising [] = []
rising [v] = [v]
rising (x:y:ls) 
  | x > y = rising (y:ls)
  | y >= x = x : rising (y:ls) 