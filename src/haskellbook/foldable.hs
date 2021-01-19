import Data.Foldable
import Data.Monoid

foldalla = foldMap All [True, False, True]
-- All {getAll = False}
foldallb = foldMap All [True, True , True]
-- All {getAll = True}

mysum :: (Foldable t, Num a) => t a -> a
mysum fld = getSum (foldMap Sum $ fld)

myproduct :: (Foldable t, Num a) => t a -> a
myproduct d = getProduct (foldMap Product d)

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem = any . (==)

-- myminimum xs = foldr (min) (1/0) xs
-- 9223372036854775807 is the highest max bound for Int
-- myminimum xs = foldr (min) (9223372036854775807) xs
myminimum [] = Nothing
myminimum xs = Just (foldr (min) (head xs))

-- the official implementation using a newtype and coercible, found in utilities
-- minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
--     getMin . foldMap (Min #. (Just :: a -> Maybe a))

mymaximum [] = Nothing
mymaximum xs = Just (foldr max 0 xs)

mynull :: (Foldable t, Eq (t a), Monoid (t a)) => t a -> Bool
mynull xs 
  | xs == mempty = True
  | otherwise = False