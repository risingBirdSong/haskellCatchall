import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
-- import qualified Data.ByteString.Char8 as BC
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- import qualified Data.Text.Lazy as TL
-- import qualified Database.Redis as R
-- import Network.URI (URI, parseURI)
-- import Web.Scotty
import qualified System.Random as SR

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  -- let maxIndex :: Int
  let maxIndex = length xs - 1 :: Int
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)


shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)
