import System.CPUTime

main = do 
  a <-  getCPUTime
  let thelast = last [1..100000000]
  b <-  getCPUTime 
  print a
  print b
  print thelast
  putStr "done" 

