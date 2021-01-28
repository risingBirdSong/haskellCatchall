import Control.Monad
import Control.Monad.Trans.State

-- newtype MyState s a = MyState {runState :: s -> (a,s)}

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []
-- addResult :: Integer -> StateT [String] 
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
-- Note that State is a type alias of StateT you imported.
main :: IO ()
-- main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
main = mapM_ putStrLn $ fizzbuzzList [100,99..1]
