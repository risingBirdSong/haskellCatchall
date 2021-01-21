{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative


type TI = []
main = do
  let trigger = undefined :: TI ([Int], [Int], [Int])
  -- let trigger = undefined :: TI (Int, Int, [Int])
  let triggera = undefined :: Identity ([Int], [Int], [Int])
  let triggerb = undefined :: MyConstant ([Int], [Int], [Int]) ([Int], [Int], [Int])
  quickBatch (traversable trigger)
  quickBatch (traversable triggerb)


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where
  pure v = Identity v  
  (<*>) (Identity f) (Identity v)  = Identity (f v  )

instance Foldable (Identity) where
  foldr f z (Identity a) = f a z 

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> (f x) 

instance Arbitrary a => Arbitrary (Identity a) where 
  -- arbitrary = do 
  -- a <- arbitrary
  -- return (Identity a)
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where 
  (=-=) = eq


newtype MyConstant a b =
  MyConstant { getMyConstant :: a } deriving (Show, Eq)

instance Functor (MyConstant a) where 
  fmap f (MyConstant a) = MyConstant a

instance (Monoid a) => Applicative (MyConstant a) where 
  pure x = MyConstant mempty
  (<*>) (MyConstant x) (MyConstant y) = MyConstant  (x <> y)

instance (Monoid a) => Foldable (MyConstant a) where
  foldMap _ _ = mempty

instance (Monoid a) => Traversable (MyConstant a) where
  traverse f (MyConstant a) = pure $ MyConstant a 

instance (Arbitrary a) => Arbitrary (MyConstant a b) where 
  arbitrary = MyConstant <$> arbitrary

instance (Eq a) => EqProp (MyConstant a b) where 
  (=-=) = eq

data Optional a =
  Nada
  | Yep a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Optional a) where 
  (<>) Nada _ = Nada
  (<>)  _ Nada = Nada
  (<>) (Yep a) (Yep b) = Yep (a<>b)

instance (Semigroup a) => Monoid (Optional a) where 
  mempty = Nada

instance Functor (Optional) where 
  fmap f Nada = Nada 
  fmap f (Yep a) = Yep (f a)

instance Applicative (Optional) where 
  pure x = Yep x
  (<*>) Nada _ = Nada
  (<*>) _ Nada = Nada
  (<*>) (Yep f) (Yep a) = Yep (f a)

instance Foldable (Optional) where
  foldr f ac (Nada) = ac 
  foldr f ac (Yep x) = f x ac 

instance Traversable (Optional) where 
  -- traverse f (Yep a) = f (Yep a) cannot construct infinite type
  traverse f (Yep a) = Yep <$> (f a)
  traverse f Nada =  pure Nada 

instance Arbitrary a => Arbitrary (Optional a) where 
  arbitrary = frequency [(1, pure Nada), (5, Yep <$> arbitrary )]

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- ex :: Int 

-- testing Optional applicative 
t_o_a = quickBatch $ applicative (undefined :: Optional (String, String , String ))

-- testing Optional Traversable 
t_o_t = quickBatch $ traversable (undefined :: Optional (String, String, String))



data List a =
  Nil
  | Cons a (List a) deriving (Show, Eq, Ord)

-- fixed version thanks to digi :) 
instance Semigroup (List a) where 
  (<>) Nil _ = Nil 
  (<>) _ Nil = Nil 
  (<>) (Cons x xs) ys = Cons (x) (xs <> ys)

-- broken version 
-- instance (Monoid a) =>  Semigroup (List a) where 
--   (<>) Nil _ = Nil 
--   (<>) _ Nil = Nil 
-- --   (<>) (Cons x xs) (Cons y ys) = Cons (x <> y) (xs <> ys)

-- <interactive>:303:22: error:
--     * No instance for (Semigroup (List b)) arising from a use of `<>'
--     * In the expression: (fmap f xs) <> (fs <*> xs)
--       In an equation for `<*>': Cons f fs <*> xs = (fmap f xs) <> (fs <*> xs)
--       In the instance declaration for `Applicative List'

genList :: Arbitrary a => Gen (List a)
genList  = do 
  a <- arbitrary
  lst <- genList
  frequency [(1, pure Nil), (5, pure (Cons a lst))]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

-- instance Arbitrary a => Arbitrary (List a) where
--   arbitrary = do 
--   a <- arbitrary
--   elements [Nil, Cons (a) Nil]

instance (Eq a) => EqProp (List a) where (=-=) = eq

-- testing semigroup List - passing :)
-- quickBatch $ semigroup (undefined :: (List String , Int))

-- semigroup:
--   associativity: +++ OK, passed 500 tests.
--   sconcat:       +++ OK, passed 500 tests.
--   stimes:        +++ OK, passed 500 tests.

instance Monoid a => Monoid (List a) where 
  mempty = Nil

instance Functor (List) where
  fmap f Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

--  quickBatch $ functor (undefined :: (List (String , String , String )))

-- functor:
--   identity: +++ OK, passed 500 tests.
--   compose:  +++ OK, passed 500 tests.

instance Applicative (List ) where 
  pure x = Cons x Nil
  (<*>) Nil _ = Nil 
  (<*>) (Cons f fs) (xs) = (fmap f xs) <> (fs <*> xs)

instance Foldable (List) where
  foldr _ b Nil = b 
  foldr f b (Cons x xs) = foldr f (f x b) xs 

*Main> quickBatch $ foldable (undefined :: List (String, String, String, Int, String))

-- Foldable:
--   foldr and foldMap: +++ OK, passed 500 tests.
--   foldl and foldMap: +++ OK, passed 500 tests.
--   fold and foldMap:  +++ OK, passed 500 tests.
--   length:            +++ OK, passed 500 tests.
--   foldMap':          +++ OK, passed 500 tests.
--   foldr':            +++ OK, passed 500 tests.
--   foldl':            +++ OK, passed 500 tests.
--   foldr1:            +++ OK, passed 500 tests; 113 discarded.
--   foldl1:            +++ OK, passed 500 tests; 83 discarded.
--   toList:            +++ OK, passed 500 tests.
--   null:              +++ OK, passed 500 tests.
--   elem:              +++ OK, passed 500 tests.
--   maximum:           +++ OK, passed 500 tests; 97 discarded.
--   minimum:           +++ OK, passed 500 tests; 93 discarded.
--   sum:               +++ OK, passed 500 tests.
--   product:           +++ OK, passed 500 tests.

-- instance Traversable (List) where 
  