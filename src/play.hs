import Data.List.Split
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

newList = insertInto 33 myList  -- Cons 1 (Cons 2 (Cons 3 (Cons 99 Nil)))
sumNewList = sum' newList -- 105

anotherNewList = insertInto 99 newList -- Cons 1 (Cons 2 (Cons 3 (Cons 99 (Cons 777 Nil))))

add x y = x + y

(reduce f x) Nil = x
(reduce f x) (Cons v l) = f v ((reduce f x ) l)

rdcTest = (reduce add 0) (anotherNewList) -- 138
rdc f acc list = (reduce f acc) (list) 
-- rdcd_a = add 0 anotherNewList
rdcTesta = rdc add 0 anotherNewList -- 138