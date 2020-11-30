data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show , Eq)


data Date = Date DayOfWeek Int deriving (Show, Eq)

day1 = Date Mon 1 
dayOne = Date Mon 1
day2 = Date Tue 2

-- instance Eq Date where 
--   (==) (Date weekDay dayNum)
--        (Date weekDay' dayNum')
--        = weekDay == weekDay' && dayNum == dayNum'