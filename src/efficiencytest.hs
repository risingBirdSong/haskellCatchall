{-# LANGUAGE BangPatterns #-}

import System.CPUTime
import Control.Exception
import Control.DeepSeq 

main = do 
  start <-  getCPUTime
  let !thelast = last [1..1000000]
  end <- getCPUTime 
  print (end - start)
  putStr "done" 

