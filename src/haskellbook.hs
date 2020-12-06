
{-# LANGUAGE NoMonomorphismRestriction #-}

-- let vs where

-- import Data.String.Utils
import Data.List

plusTwowhere n = print $ f n 
    where f n = n + 2 

plusTwolet n = let f n = 2 + n  
                in print $ f n 


aaa = let x = 333; y = 666 in x + y -- 999
aab :: Integer
aab = let x = 222; y = 333 in x * y -- 73926

aac = x * y where x = 67; y = 91 -- 6097

aad = x + y where x = 213; y = 4234 -- 4447

myGreeting :: String
myGreeting = (++) "hello" " world!"

main :: IO ()
main = do
putStrLn myGreeting
putStrLn secondGreeting
  where secondGreeting = "hi again"


aae =  'c' : "hris"
aaf = 'P' : ""


crry = "Curry is awesome"
aag = take 1 $ drop 4 crry -- "y"

takerdroper t d s= take t $ drop d s 

thirdLetter :: String -> Char
thirdLetter str = str !! 2 

letterIndex idx = "curry is awesome" !! idx 

rvrswrds str = unwords $ reverse $ words str

data Mood = Blah | Woot deriving (Show , Eq)

notM :: Mood -> Mood
notM m 
  | m == Blah = Woot 
  |otherwise = Blah

changeMood :: Mood -> Mood 
changeMood Blah = Woot 
changeMood _ = Blah 


-- book challenge pg 142
-- 6 / length [1, 2, 3]
-- this one is interesting because 6 / 2 works -> 3.0
-- but the length doesnt work because it returns type int which wont work with /
-- here is the fix
aah = 6 `div` length [1,2,3]
-- Chapter 4 Exercises Start
isPali str = str == reverse str

myAbs n = if n > 0 then n else negate n

tuplea :: (a, b) -> (c, d) -> ((b, d), (a, c))
tuplea a b = ((snd a, snd b), (fst a, fst b))

oneMoreThanLength xs = lngth + 1 where lngth = length xs

l x = x

myHead xs = xs !! 0 

myTupleHead = \ (x,y) -> x

-- 1. Which of the following types is the type of show?
-- a) show a => a -> String
-- b) Show a -> a -> String
-- c) Show a => a -> String
-- answer c


-- 2. Which of the following types is the type of (==)?
-- a) a -> a -> Bool
-- b) Eq a => a -> a -> Bool
-- c) Eq a -> a -> a -> Bool
-- answer c 

-- Which of the following types is the type of fst?
-- (a, b) -> a

-- 4. Which of the following types is the type of (+)?
-- d) (+) :: Num a => a -> a -> a


-- Chapter 4 Exercises End


-- Exercises: Parametricity , pg 140 / 172


aai :: a -> a -> a
aai a b = a

aaj :: a -> a -> a 
aaj a b = b 

aak :: a -> b -> b 
aak a b = b 

aal = 1

aam =  (* 9) 6
aan =  head [(0,"doge"),(1,"kitteh")]

aao =  head [(0 :: Integer ,"doge"),(1,"kitteh")]

aap = if False then True else False
aaq = length [1, 2, 3, 4, 5]

aar = (length [1, 2, 3, 4]) > (length "TACOCAT")


i :: a -> a
i a = a 


data Woot
data Blah
f :: Woot -> Blah
f = undefined
g :: (Blah, Woot) -> (Blah, Blah)
g (a , b) = (a, f b) 


-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         check' :: forall a. a -> a -> Bool
check' :: Eq a => a -> a -> Bool
check' a a' = a == a'


x :: Int -> Int
x blah = blah + 20

-- printIt :: IO ()
printIt :: Show a => a -> IO ()
printIt x = putStrLn (show x)

-- the fix was deriving show
data Person = Person Bool deriving Show 
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood_ = Blah_
  | Woot_ deriving (Show , Eq, Ord)

-- ah! i thought that this would work, but I missed that it also needs to derive Eq
settleDown :: Mood_ -> Mood_
settleDown x = if x == Woot_
  then Blah_
  else x


type Subject = String
type Subject_ = String
type Verb = String
type Verb_ = String
type Object = String
data Sentence = Sentence Subject Verb Object | Sentence_ Subject_ Verb_
  deriving (Eq, Show)

-- s1 = Sentence "dogs" "drool"
-- s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)
-- Papu (Rocks "hey") (Yeah True)
-- Papu (Rocks "hey") (Yeah True)



chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a  

-- all equivalent
mTha x y z = x * y * z
mThb x y = \z -> x * y * z
mThc x = \y -> \z -> x * y * z
mThd = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (+ 1)

-- addFive = ((\x y -> if x > y then y else x))
addFive = (\x y -> (if x > y then y + 5 else x))
addFive_ a b = 5 + (\x y -> if x > y then y else x) a b

mflip f x y = f y x

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

 
tupling :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
tupling (a, b, c) (d , e , f) = ((a, d), (c, f))
-- tupling (1,2,3) (4,5,6)
-- ((1,4),(3,6))

-- case practice 
-- functionC x y = if (x > y) then x else y

functionC_case x y =
  case x > y of
    True -> x
    False -> y  

-- ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2 n = 
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1


myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f a b = f b a 

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' a b c d = d 

-- returnBroke :: (((a -> b) -> c) -> d) -> d
-- returnBroke _ _ _ d = d
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
 
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.59 = 'D'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y < 0.59 = 'F'
  where y = x / 100

-- 4. What types of arguments can pal take?
-- I was right thinking it's a generic list [a], but In addition to my thought
-- the as must be constrained by the Eq class 
pal xs
  | xs == reverse xs = True
  | otherwise = False


numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1


-- great explanations of $ and . from book! The clearest explanation for me and I understand them much better now
cmpseA  = negate . sum $ [1, 2, 3, 4, 5]
cmpseAA ls = negate . sum $ ls
cmpseB =  take 5 . reverse $ [1..10]

cmpseC = take 5 . enumFrom $ 10

cmposeD =  take 5 . filter odd . enumFrom $ 7
cmposeE = take 10 . filter odd $ [7..]
-- c) an indication of whether its argument is a positive or negative number or zero

pntFreeCmpseA = negate . sum

ff :: Int -> [Int] -> Int
ff = foldr (+) 

add :: Int -> Int -> Int
add x y = x + y
addPF :: Int -> Int -> Int
addPF = (+)
addOne :: Int -> Int
addOne = \x -> x + 1
addOnePF :: Int -> Int
addOnePF = (+1)

mainy :: IO ()
mainy = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0)
  print ((addOnePF . addOne) 0)
  print ((addOne . addOnePF) 0)
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0))
  print ((negate . addOne) 0)
  print ((addOne . addOne . addOne . negate . addOne) 0)

tensDigit x = d 
  where xLast = x `div` 10
        d = xLast `mod` 10
-- tnsDgt with divMod
tnsDgt n = (\x -> read x :: Int) . (:[]) . last $ show . fst $ n `divMod` 10

tnsDgtAgain n = fst $ snd (n `divMod` 100) `divMod` 10 

hunsD x = (x `div` 100) `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y first 
  | first = x
  | otherwise = y

foldBoolCase x y first =
  case first of 
    True -> x 
    False -> y 

gg :: (a -> b) -> (a, c) -> (b, c)
gg f (a,c) = (f a, c)

-- *Main> show (1,2)
-- "(1,2)"
-- *Main> read "(1,2)" :: (Int, Int)
-- (1,2)
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)


-- 5. Next, write a pointfree version of roundTrip. (n.b., this refers to
-- the function definition, not to its application in main)

rndTrpPF :: (Show a, Read a) => a -> a
rndTrpPF = read . show 

rndTrpPF_ = (\x -> read x :: Int) . show

add' :: (Int, Int) -> Int
add' (x, y) = x + y

curriedAdd = curry add'



nestedComp :: Num b => [[b]] -> [[b]]
nestedComp xxs = [ map (*2) x | x <- xxs]

testComp xxs = [y * 2 | x <- xxs, y <- x]

nestedData = [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7,8],[8,9],[9,10]]


ff' True = Just 1
ff' _ = Nothing

digitLookup n 
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "???"

digitsToList n = reverse $ go n
  where go 0 = [] 
        go n = n `mod` 10 : go (n `div` 10)

digitsSolve n = intercalate "-" $ map digitLookup $ digitsToList n

recursiveSum n = go n 0
  where go 0 acc = acc 
        go n acc = go (n - 1) (acc + n) 

multBySum x y = go x y 0 
  where go 0 _ acc = acc 
        go x y acc = go (x-1) y (acc + y)


myHead' [] = Nothing
myHead' (x:xs) = Just x

myTail' [] = []
myTail' (x:xs) = xs

myRange start stop = go start stop []
  where go start stop acc 
          | start == stop = acc 
          | start < stop = go (start + 1) stop (acc ++ [start])
          | start > stop = go (start - 1) stop (acc ++ [start])

-- "all i wanna do is have some fun"
myWords words = go words []
  where go [] acc = acc 
        go words acc = go (drop 1 (dropWhile (/=' ') words)) (acc ++ [takeWhile (/=' ') words])

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
  ++ thirdSen ++ fourthSen

myLines words = reverse $ go words []
  where go [] acc = acc 
        go words acc = go (drop 1(dropWhile (/='\n') words)) ((takeWhile (/='\n') words) :acc)

-- Hey why is does this work?
-- My understanding was Char can only be a single character. Is this a special exemption because of the escape character \ ? 

-- \n is a single character, the compiler translates it into ascii 10
-- \n is just the name given to it so it can easily be typed

dropping ls = drop 1 $ dropWhile (/='\n') ls
taking ls = takeWhile (/='\n') ls
-- droppingLtr ls = dropWhile (/='ab') ls


shouldEqual = [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

squares = [x^2|x<-[1..5]]
cubes = [x^3|x<-[1..5]]
sqCbTups = [(s, c) | s <- squares, c <- cubes]
sqCbTupsSml = [(s, c) | s <- squares, c <- cubes, s < 50, c < 50]
sqCbTupsSmlLength = length  [(s, c) | s <- squares, c <- cubes, s < 50, c < 50]