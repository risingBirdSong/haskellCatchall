{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
import Data.List
import Test.QuickCheck

listOrdered :: (Foldable t, Ord a) => t a -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)


qcOrderedLists :: (Ord a) => forall a. IO ()
qcOrderedLists = quickCheck ((listOrdered :: [a] -> Bool) . sort)

-- And then use it like this
myTest = do
  qcOrderedLists @Int
  qcOrderedLists @String
  qcOrderedLists @Float
  -- etc.