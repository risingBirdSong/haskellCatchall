import Data.List 

-- https://app.codesignal.com/arcade/graphs-arcade/kingdom-roads/nCMisf4ZKpDLdHevE

-- newRoadSystem roadRegister = 

mtrxA =[[False, True,  False, False],
        [False, False, True,  False],
        [True,  False, False, True ],
        [False, False, True,  False]] 


allTrues boolList = filter (\x -> x == True) boolList

newRoadSystem mtrx = all (\b -> b == True) $ zipWith (\ a b -> allTrues a == allTrues b ) mtrx (transpose mtrx)