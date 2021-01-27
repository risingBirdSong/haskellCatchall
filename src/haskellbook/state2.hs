import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random



data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)


intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix

-- rollDie :: State StdGen Die
-- rollDie = state $ do
-- (n, s) <- randomR (1, 6)
-- return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie' rollDie' rollDie'

evalled = evalState rollDieThreeTimes' (mkStdGen 0)
-- (DieSix,DieSix,DieFour)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie'

-- *Main> take 10 $ evalState infiniteDie  (mkStdGen 3)
-- [DieSix,DieSix,DieSix,DieSix,DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

-- *Main> evalState (nDie 4) (mkStdGen 4)
-- [DieSix,DieThree,DieThree,DieTwo]

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 100 = count
          | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1) nextGen

type SumOfDice = Int 
type CountOfRolls = Int 
-- rlsItTakes :: StdGen -> (a -> Bool) -> Int 
rlsItTakes g f = go 0 0 g   
  where go :: SumOfDice -> CountOfRolls -> StdGen -> Int
        go sm cnt gn 
          | f sm = cnt 
          | otherwise = 
          let (die, nextGen) = randomR (1,6) gn 
          in go (sm + die) (cnt + 1) nextGen

rlsItTakesWithHistory g f = go 0 0 [] g   
  where go :: SumOfDice -> CountOfRolls -> [Int] -> StdGen -> ( [Int] , Int)
        go sm cnt hst gn 
          | f sm = (hst, cnt)
          | otherwise = 
          let (die, nextGen) = randomR (1,6) gn 
          in go (sm + die) (cnt + 1) (die : hst) nextGen
-- We can also use randomIO, which uses IO to get a new value each
-- time without needing to create a unique value for the StdGen:
-- Prelude> :t randomIO
-- randomIO :: Random a => IO a
-- Prelude> (rollsToGetTwenty . mkStdGen) <$> randomIO
-- 6
-- Prelude> (rollsToGetTwenty . mkStdGen) <$> randomIO
-- 7
