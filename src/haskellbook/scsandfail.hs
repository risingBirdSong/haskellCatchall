import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
  Failure' e
  | Success' a
  deriving (Eq, Show)
-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e 
  fmap f (Success' v) = Success' (f v) 
-- This is different
instance Monoid e => Applicative (Validation e) where
  pure v = Success' v 
  (<*>) (Success' f) (Success' v) = Success' (f v) 
  (<*>) (Failure' e) (Success' v) = Failure' e 
  (<*>) (Success' f) (Failure' e) = Failure' e 
  (<*>) (Failure' e) (Failure' ee) = Failure' (e <> ee) 

-- *Main> pure 3 :: Validation String Int
-- Success' 3

genValidation :: (Arbitrary a, Arbitrary b) => Gen (Validation a b)
genValidation = do 
  aa <- arbitrary
  bb <- arbitrary
  elements [Success' aa, Failure' bb]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where 
  arbitrary = genValidation

instance (Eq a, Eq b) => EqProp (Validation a b) where 
  (=-=) = eq


triggerA :: Validation String ([Int], String, String)
triggerA = undefined

triggerB :: Validation [Int] (String, String, String)
triggerB = undefined

triggerC :: (Num a) => Validation String (Sum a, Sum a, Sum a)
triggerC = undefined

