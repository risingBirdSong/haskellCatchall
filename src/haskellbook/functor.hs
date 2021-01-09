{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Test.QuickCheck.Function
import Debug.Trace
import Data.Semigroup

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

data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where 
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where 
  fmap f (Four' a aa aaa b) = Four' a aa aaa (f b)

-- 8. Can you implement one for this type? Why? Why not?
data Trivial = Trivial
-- no, because Trivial is kind *, not * -> * 


incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)
showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where 
  fmap f LolNope = LolNope
  fmap f (Yeppers x) = Yeppers (f x) 

data MySum a b =
  MyFirst a
  | MySecond b
  deriving (Eq, Show)
instance Functor (MySum a) where
  fmap f (MyFirst e) = MyFirst e
  fmap f (MySecond x) = MySecond (f x)

data TestPair a b = TestPair a b 

-- instance Functor (TestPair a) where 
--   fmap f (TestPair a b) = TestPair (f a) (f b)  

--   applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
-- 2. Why is a Functor instance that applies the function only to First,
-- Either’s Left, impossible? We covered this earlier.

-- because a has been partially applied to TestPair to get the right kindedness
-- so its baked into the Functor (the f here)


-- data Bool =
  -- False | True

-- false

data BoolAndSomethingElse a =
  False' a | True' a

-- true 

-- data BoolAndMaybeSomethingElse a =
  -- Falsish | Truish a

--true 

newtype Mu f = InF { outF :: f (Mu f) }
-- i dont think so 

-- data D =
  -- D (Array Word Word) Int Int
-- no, or at least it wouldnt be that useful

data Summy a b =
  Firsty a
  | Secondy b deriving (Show, Eq)

instance Functor (Summy e) where
  fmap f (Firsty a) = Firsty a
  fmap f (Secondy b) = Secondy (f b)


data More b a =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R a b a') = R (a) (f b) (a')

-- Prelude> fmap (+1) (L 1 2 3)
-- L 2 2 4
-- Prelude> fmap (+1) (R 1 2 3)
-- R 1 3 3

data Quant a b =
  Finance
  | Desk a
  | Bloor b deriving Show

instance Functor (Quant a) where 
  fmap _ Finance = Finance
  fmap f (Desk a) = (Desk a)
  fmap f (Bloor b) = Bloor (f b)

data K a b =
  K a deriving (Show, Eq)

instance Functor (K a) where 
  fmap _ (K a) = K a

data Tuple a b =
  Tuple a b
  deriving (Eq, Show)
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)
-- this actually works, goofy as it looks.
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b


newtype K' a b =
  K' a deriving Show

instance Functor (Flip K' a) where 
  fmap f (Flip (K' a)) = Flip $ K' (f a)

data EvilGoateeConst a b =
  GoatyConst b deriving Show

instance Functor (EvilGoateeConst a) where 
  fmap f (GoatyConst b ) = GoatyConst (f b)

data LiftItOut f a =
  LiftItOut (f a) deriving Show

instance (Functor f) => Functor (LiftItOut f) where 
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving Show 

instance (Functor g) => Functor (IgnoreOne f g a) where  
  fmap f (IgnoringSomething fa gb) = IgnoringSomething (fa) (fmap f gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t) deriving Show 

instance (Functor g) => Functor (Notorious g o a) where 
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt) 

data List a =
  Nil
  | Cons a (List a)

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving Show

instance Functor GoatLord where 
  fmap f (NoGoat) = NoGoat
  fmap f (OneGoat aa) = OneGoat (f aa)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x)(fmap f y)(fmap f z)

recursiveGoatTestA = fmap (+1) (OneGoat 1)
recursiveGoatTestB = fmap (+1) (MoreGoats (OneGoat 1)(OneGoat 2)(OneGoat 3))

data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor (TalkToMe) where
  fmap f (Halt) = Halt
  fmap f (Print s a) = Print s (f a) 
  fmap f (Read (sa)) = Read (f . sa) 