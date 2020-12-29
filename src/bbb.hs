import System.Random
import System.Random.Stateful
import Data.List


rolls :: RandomGen g => Int -> g -> [Word]
rolls n = take n . unfoldr (Just . uniformR (1, 6))
pureGen = mkStdGen 137
-- rolling = rolls 10 