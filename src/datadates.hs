data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
instance Show DayOfWeek where 
  show Mon = "monday"
  show Tue = "tuesday"
  show Weds = "wednesday"
  show Thu = "thursday"
  show Fri = "friday"
  show Sat = "saturday"
  show Sun = "sunday"

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