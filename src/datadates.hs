data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show , Eq)


data Date = Date DayOfWeek Int deriving (Eq)

instance Show Date where 
  show (Date Mon 1) = "monday the first"
  show (Date Tue 2) = "tuesday the second"

dayOne = Date Mon 1
dayTwo = Date Tue 2