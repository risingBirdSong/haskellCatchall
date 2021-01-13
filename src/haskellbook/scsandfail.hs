data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)
-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e 
  fmap f (Success v) = Success (f v) 
-- This is different
instance Monoid e => Applicative (Validation e) where
  pure v = Success v 
  (<*>) (Success f) (Success v) = Success (f v) 
  (<*>) (Failure e) (Success v) = Failure e 
  (<*>) (Success f) (Failure e) = Failure e 
  (<*>) (Failure e) (Failure ee) = Failure (e <> ee) 

-- *Main> pure 3 :: Validation String Int
-- Success 3