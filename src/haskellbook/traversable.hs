import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TI = []
main = do
  let trigger = undefined :: TI ([Int], [Int], [Int])
  -- let trigger = undefined :: TI (Int, Int, [Int])
  let triggera = undefined :: Identity ([Int], [Int], [Int])
  quickBatch (traversable trigger)
  quickBatch (traversable triggera)


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