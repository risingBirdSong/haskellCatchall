import Data.Time
import Data.Fixed
import Control.Applicative
import Data.Time.Format.ISO8601


-- following along to https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html

myGetCurrentTime :: IO UTCTime
myGetCurrentTime = getCurrentTime



-- data UTCTime = UTCTime
--   { utctDay     :: T.Day       -- calendar day
--   , utctDayTime :: T.DiffTime  -- seconds from midnight
--   } deriving Show

-- *Main> T.getCurrentTime
-- 2021-04-21 19:06:04.011026 UTC

learningMonad = do 
    today <- utctDay <$> getCurrentTime
    let todayGregorian = toGregorian today
    putStrLn (show todayGregorian)
    -- (2021,4,21)
    return ()


-- mkUTCTime :: (Integer, Int, Int)
--           -> (Int, Int, Pico)
--           -> UTCTime
mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

-- *Main> mkUTCTime (2019, 9, 1) (15, 13, 0)
-- 2019-09-01 15:13:00 UTC

midnight' = liftA3 (,,) todHour todMin todSec $ midnight
midday' = liftA3 (,,) todHour todMin todSec $ midday

formatting = do 
  now <- getCurrentTime
  print now
  let formatA = formatTime defaultTimeLocale "%Y-%m-%d" now  
  putStrLn formatA
  let formatB = formatTime defaultTimeLocale  "%H:%M:%S" now
  putStrLn formatB
  return ()

-- %y: year, 2-digit abbreviation
-- %Y: year, full
-- %m: month, 2-digit
-- %d: day of month, 2-digit
-- %H: hour, 2-digit, 24-hour clock
-- %I: hour, 2-digit, 12-hour clock
-- %M: minute, 2-digit
-- %S: second, 2-digit
-- %p: AM/PM
-- %z: timezone offset
-- %Z: timezone name

--  If you don’t want the zero-padding of the specific component, you can add a dash between
--  the percent sign and the directive, e.g. a format string of "%H:%M" would give "04:10" 
--  but a format string of "%-H:%M" would give "4:10".
-- https://hackage.haskell.org/package/time-1.8.0.2/docs/Data-Time-Format.html#v:formatTime


parseTestA =  parseTimeM True defaultTimeLocale "%Y-%m-%d" "2019-08-31" :: Maybe Day
parseTestB = parseTimeM True defaultTimeLocale "%Y-%m-%d" "asdf" :: Maybe Day
-- *Main> parseTestA 
-- Just 2019-08-31
-- *Main> parseTestB 
-- Nothing

addingDaysLearn = do 
  today <- utctDay <$> getCurrentTime
  putStrLn $ ("today is -> ") ++ (show today)
  putStrLn "type a number of days you want to add on the next line, then enter"
  numAsString <- getLine 
  let num = read numAsString :: Integer
  print ("add " ++ (show num) ++ "days to today")
  print (addDays num today)
  return ()


-- parseStringAsDate :: IO ()
parseStringAsDate = do 
  txt <- getLine
  -- let parsed = formatTime defaultTimeLocale   
  return ()

timeFormat = "%Y-%m-%d %H:%M:%S"
handleTime = parseTimeOrError True defaultTimeLocale timeFormat

isoTimeNothing = formatParseM iso8601Format "2012-01-01 13:08:14 UTC" :: Maybe UTCTime
isoTimeJust = formatParseM iso8601Format "2012-01-01T13:08:14Z" :: Maybe UTCTime

brokenTime :: UTCTime 
brokenTime = handleTime "2012-01-01T13:08:14Z"

workingTime :: UTCTime 
workingTime = handleTime "2012-01-01 13:08:14"

aConvenienceParse = iso8601ParseM  "2012-01-01T13:08:14Z" :: IO UTCTime
convenientlyBroke = iso8601ParseM  "broke" :: IO UTCTime
-- *** Exception: user error (no parse of "broke")
-- "2012-01-01T13:08:14Z"

f :: Maybe Int
f = do
  -- Right res <- pure $ Left "haha, no"
  -- returns Nothing
  Right res <- pure $ Right 2
  -- returns (Just 2)
  pure (res)


parseIsoTime str = formatParseM iso8601Format str :: Maybe UTCTime

timeA = parseIsoTime "2012-01-01T13:08:14Z"
timeB =  parseIsoTime "2012-02-01T13:08:14Z"

timeCompare (Just a) (Just b) = a `compare` b


main = putStrLn "hey"