-- import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Sum n = Sum n deriving (Show, Eq)

instance Num n => Semigroup (Sum n) where
  Sum x <> Sum y = Sum (x + y)

instance Num n => Monoid (Sum n) where
  mempty = Sum 0

instance (Arbitrary n, Num n) => Arbitrary (Sum n) where 
  arbitrary = do 
  a <- arbitrary
  return (Sum a)

instance (Eq n) => EqProp (Sum n) where (=-=) = eq
