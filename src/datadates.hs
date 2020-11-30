data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Show)

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where 
  Mon == Mon = True 
  Tue == Tue = True 
  Weds == Weds = True 
  Thu == Thu = True 
  Fri == Fri = True 
  Sat == Sat = True 
  Sun == Sun = True
  (==) _  _ = False

instance Eq Date where 
  (==) (Date weekDay dayNum)
       (Date weekDay' dayNum')
       = weekDay == weekDay' && dayNum == dayNum'