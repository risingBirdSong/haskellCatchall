module Addition where
import Test.Hspec
import Test.QuickCheck
import Data.List
import Debug.Trace

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
  it "1 + 1 is greater than 1" $ do
    (1 + 1) > 1 `shouldBe` True
  it "2 + 2 is equal to 4" $ do
    2 + 2 `shouldBe` 4
  it "x + 1 is always greater than x" $ do
    property $ \x -> x + 1 > (x :: Int)

moreaddition = hspec $ do
  describe "more addition" $ do
  it "4 + 4 is less than 10" $ do
  (4 + 4 ) < 10 `shouldBe` True

intentionallyfailedtest = hspec $ do
  describe "failed addition" $ do
    it "2 + 2 should equal 4" $ do
    (2 + 2) == 4 `shouldBe` False

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

maina :: IO ()
maina = hspec $ do
    describe "Addition" $ do
    it "15 divided by 3 is 5" $ do
       dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
        dividedBy 22 5 `shouldBe` (4, 2)


sayHello :: IO ()
sayHello = putStrLn "hello!"

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]


oneThroughThreeBiased :: Gen Int
oneThroughThreeBiased = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)


-- *Addition>  sample (genTuple :: Gen (Int, Char))
-- (0,'J')
-- (-2,'=')
-- (-1,':')
-- (-6,'J')
-- (-8,'\SO')
-- (4,'8')
-- (-7,'!')
-- (-4,'L')
-- (0,'\475775')
-- (15,'S')
-- (-14,'R')
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- *Addition> sample (genEither :: Gen (Either Int Char))
-- Left 0
-- Left 2
-- Left 3
-- Right '\531073'
-- Right 'D'
-- Right '\938761'
-- Left 9
-- Right '\SI'
-- Right 'K'
-- Left 3
-- Right '.'

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- *Addition> sample (genMaybe :: Gen (Maybe Char))
-- Nothing
-- Nothing
-- Just 'W'
-- Nothing
-- Just '\221149'
-- Just 'Q'
-- Nothing
-- Nothing
-- Nothing
-- Just '$'
-- Just 'Y'

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
      a <- arbitrary
      frequency [ (1, return Nothing)
            , (5, return (Just a))]

-- *Addition> sample (genMaybe' :: Gen (Maybe Int))
-- Just 0
-- Just (-1)
-- Nothing
-- Just (-4)
-- Just (-4)
-- Nothing
-- Just 12
-- Just 6
-- Nothing
-- Just 11
-- Just (-2)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
runQc :: IO ()
runQc = quickCheck prop_additionGreater

testhalf :: Int -> Bool 
testhalf x = (abs x) `div` 2 <= (abs x)

qcHalf :: IO ()
qcHalf = quickCheck testhalf 

halfAnddouble = (*2) .(/2)
halfAnddoublepointed x = 2 * (x/2) 
halfidentity :: Double -> Bool
halfidentity x = x == (halfAnddouble x)

qcHalfId = quickCheck halfidentity

-- listOrdered :: [Integer ] -> Bool
listOrdered :: (Foldable t, Ord a) => t a -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

qcorderedlists  = quickCheck ((listOrdered :: [Integer] -> Bool ) . sort )
qcorderedlistsa  = quickCheck ((listOrdered :: [String] -> Bool ) . sort )

assocTpl x y z = ((x + (y + z)) , ((x + y) + z))
plusAssociative x y z = (x + (y + z)) == ((x + y) + z)
qcassoc = quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
-- https://stackoverflow.com/questions/588004/is-floating-point-math-broken
-- https://stackoverflow.com/questions/33161504/how-to-calculate-floating-point-numbers-in-haskell
qcassoca = quickCheck (plusAssociative :: Float -> Float -> Float -> Bool)
qcassocb = quickCheck (plusAssociative :: Rational -> Rational -> Rational -> Bool)

plusCommutative x y = x + y == y + x
qcpca = quickCheck (plusCommutative :: Int -> Int -> Bool)
qcpcb = quickCheck (plusCommutative :: Float -> Float -> Bool)

-- qoutRemEqual 0 0 = True  
qoutRemEqual _ 0 = True  
qoutRemEqual x y = (quot x y)*y + (rem x y) == x 
qoutRemTest = quickCheck (qoutRemEqual :: Integer -> Integer -> Bool)

genPosNum :: Gen Int
genPosNum = elements [1..1000]

ttt = quickCheck (forAll genPosNum qoutRemEqual)

pwrComm x y = x ^ y == y ^ x
-- pwrCommTest = quickCheck (pwrComm :: Int -> Int -> Bool) fails, understandably
pwrAssoc x y z = trace  (show $ (x ^ y) ^ z) (x ^ y) ^ z == trace (show $ x ^ (y ^ z)) x ^ (y ^ z)
-- fails 
powerAssocTest = quickCheck (pwrAssoc :: Int -> Int -> Int -> Bool)

revId xs = (reverse . reverse $ xs) == id xs
revIdTest = quickCheck (revId :: [Int] -> Bool)


folded xs ys = foldr (:) ys xs
appended xs ys = xs ++ ys

foldedAndAppended xs ys = (folded xs ys) == (appended xs ys)
foldedAndAppendedTest = quickCheck (foldedAndAppended :: [Int] -> [Int] -> Bool)

myconcat xs = foldr (++) [] xs
concatting xs = (concat xs) == (myconcat xs) 
concattingTest = quickCheck (concatting :: [[Int]] -> Bool)

f n = length (take n [1..]) == n
fTest = quickCheck (f :: Int -> Bool)

yyy = quickCheck $ forAll genPosNum f 
-- ttt = quickCheck (forAll genPosNum qoutRemEqual)
