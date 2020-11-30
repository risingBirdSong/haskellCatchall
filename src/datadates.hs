data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where 
  Mon == Mon = True 
  Tue == Tue = True 
  Mon == Tue = True
  Tue == Mon = True 
  (==) _  _ = False