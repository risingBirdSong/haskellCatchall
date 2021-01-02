import System.Random
import Data.List
import Control.Monad () 
-- import System.Random.Stateful
import System.Random.Shuffle 
import Debug.Trace
import Control.Monad (replicateM, (>=>))
import Data.List (sortBy)
import Data.Function (on)

-- shuffle, shuffleTwice :: [Card] -> IO [Card]
shuffle xs 
  = fmap fst 
  . sortBy (compare `on` snd) 
  . zip xs <$> replicateM (length xs) (randomRIO (1 :: Int, 1000 :: Int))

aa = do
  g <- newStdGen
  print . take 10 $ (randomRs (1 :: Int, 10 :: Int) g)

getRandomIndex x y = do
  g <- newStdGen 
  let [rando] = take 1 $ randomRs (x, y) g
  return (rando)

-- getElement str = (ltr, delete ltr str)
--   where randoidx = getRandomIndex 0 (length str -1)
--         ltr = str !! randoidx 
                             
-- gather [] = []
-- gather str = (fst getElement str) : gather (snd  getElement str)

main  = do
  str <- getLine 
  putStrLn "keep going"

-- http://zvon.org/other/haskell/Outputlist/delete_f.html