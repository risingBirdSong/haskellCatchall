import Test.QuickCheck
import Test.QuickCheck.Function
import Debug.Trace
-- fmap (+1) (Just 1)
-- Just 2

-- *Main> fmap (fmap (+1)) [(Just 1),(Just 2),Nothing]
-- [Just 2,Just 3,Nothing]

class Sumthin a where
  s :: a -> a

class Else b where
  e :: b -> f (g a b c)

data T b = T b  

class Something a where 
  somefunction :: a -> b a -> T (b a)
  
data Wrapper a = Wrapper a

data Star = Star

-- data FixMePls =
--   FixMe
--   | Pls
--   deriving (Eq, Show)

-- instance Functor FixMePls where
--   fmap = error "it doesn't matter, it won't compile"

--     * Expected kind `* -> *', but `FixMePls' has kind `*'
--     * In the first argument of `Functor', namely `FixMePls'
--       In the instance declaration for `Functor FixMePls'
--    |
-- 27 | instance Functor FixMePls where

data FixMePls a =
  FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- In Haskell, the two use cases have been merged such that we
-- don’t call out constructor classes as being separate from typeclasses,
-- but we think it’s useful to highlight that something significant has
-- happened here. Now we have a means of talking about the contents
-- of types independently from the type that structures those contents.
-- That’s why we can have something like fmap that allows us to alter
-- the contents of a value without altering the structure (a list, or a Just)
-- around the value

-- you can't talk about Functor f without higher kinded types
-- f has to be something accepting one more parameter
-- without higher kinded types
-- like you can't even express this polymorphism

b = (fmap.fmap)(++ "lol") (Just ["Hi,", "Hello"])
bb =fmap (fmap (++ "lol")) (Just ["Hi,", "Hello"])

c = fmap (*2) (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

ee :: IO Integer
ee = let ioi = readIO "1" :: IO Integer
         changed = fmap read $ fmap ("123"++) $ fmap show ioi
     in fmap (*3) changed

-- ioi = readIO "1" :: IO Integer
-- changed = fmap (read :: [Char] -> Integer) $ fmap ("123"++) $ fmap show ioi
-- changed =  fmap  show ioi


newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)



functorIdentity :: (Functor f, Eq (f a)) =>
                          f a
                          -> Bool
functorIdentity f =
  fmap id f == f

instance (Arbitrary a) => Arbitrary (Identity a) where 
  arbitrary = do 
   a <- arbitrary
   return (Identity a)

identityCheck = quickCheck $ \x -> functorIdentity (x :: (Identity Int))


functorCompose' :: (Eq (f c), Functor f) =>
  f a
  -> Fun a b
  -> Fun b c
  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

testa = fmap ((+1).(*5)) (Identity 1) 
testb = fmap (+1) . fmap (*5) $ (Identity 1) 

-- https://github.com/isaiaholoyede/HaskellBook

type IntToInt = Fun Int Int
type IntFC = Identity Int -> IntToInt -> IntToInt -> Bool
aaa = quickCheck (functorCompose' :: IntFC)

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where 
  fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where 
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

-- *Main> fmap (+1) (Three "hello" 3 3)
-- Three "hello" 3 4
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a ) where 
  fmap f (Three' a b c) = Three' a (f b ) (f c)