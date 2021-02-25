import Data.List
import Numeric 
import Data.Char 
import qualified Data.Map as M
import qualified Data.Vector as V 
import qualified Data.Set as S 
import Control.Lens 
import Data.Maybe
import Data.List.Split
import Debug.Trace
import Control.Monad

addTwoDigits n = sum $ map digitToInt $ show n
addTwoDigits'' n = uncurry (+) $ divMod n 10 
addTwoDigits' :: Show a => a -> Int
addTwoDigits' = sum . map (\c -> read [c]) . show
addTwoDigits''' n = (mod n 10)+(div n 10)