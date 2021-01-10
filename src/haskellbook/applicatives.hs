import Control.Applicative
import Data.List

-- oooh interesting
testing = do 
  let result = (++) <$> getLine <*> getLine
  (fmap length result)
-- the fmap has reached inside IO monad

added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

-- z :: Maybe Integer
-- z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- tupled :: Maybe (Integer, Integer)
-- tupled = liftA2 (,) y z
-- tupled' =  (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed =liftA2 max' x y

xs = [1, 2, 3]
ys = [4, 5, 6]

xx :: Maybe Integer
xx = lookup 3 $ zip xs ys

yy :: Maybe Integer
yy = lookup 2 $ zip xs ys

-- summed :: Maybe Integer
summed = uncurry max <$> liftA2 (,) xx yy

