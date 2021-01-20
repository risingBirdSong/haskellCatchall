{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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