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
