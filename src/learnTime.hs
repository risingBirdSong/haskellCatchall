import qualified Data.Time as T 
import Data.Fixed
import Control.Applicative


-- following along to https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html

myGetCurrentTime :: IO T.UTCTime
myGetCurrentTime = T.getCurrentTime



-- data UTCTime = UTCTime
--   { utctDay     :: T.Day       -- calendar day
--   , utctDayTime :: T.DiffTime  -- seconds from midnight
--   } deriving Show

-- *Main> T.getCurrentTime
-- 2021-04-21 19:06:04.011026 UTC

learningMonad = do 
    today <- T.utctDay <$> T.getCurrentTime
    let todayGregorian = T.toGregorian today
    putStrLn (show todayGregorian)
    -- (2021,4,21)
    return ()


-- mkUTCTime :: (Integer, Int, Int)
--           -> (Int, Int, Pico)
--           -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  T.UTCTime (T.fromGregorian year mon day)
          (T.timeOfDayToTime (T.TimeOfDay hour min sec))

-- *Main> mkUTCTime (2019, 9, 1) (15, 13, 0)
-- 2019-09-01 15:13:00 UTC

midnight' = liftA3 (,,) T.todHour T.todMin T.todSec $ T.midnight

main = putStrLn "hey"