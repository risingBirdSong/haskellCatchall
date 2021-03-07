
myData = [2,4,6,8]

isEvenF x
    | x `rem` 2 == 0 = Just x
    | otherwise      = Nothing

myTraverse = traverse isEvenF myData
myMap = map isEvenF myData

