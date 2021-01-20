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