module Addition where
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
  it "1 + 1 is greater than 1" $ do
    (1 + 1) > 1 `shouldBe` True
  it "2 + 2 is equal to 4" $ do
    2 + 2 `shouldBe` 4
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
