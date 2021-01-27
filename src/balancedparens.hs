{-# LANGUAGE DataKinds #-}

balan [] [] = True 
balan [x] [] = False 
balan [] stk = False 
balan (q:qs) (stk) 
  | q == '(' = balan qs ('(':stk)
  | null stk = False
  | (checkMatch q (head stk)) == True = balan qs (drop 1 stk)
    | otherwise = False 
  
checkMatch x y 
  | [x , y] == ")(" = True 
  | otherwise = False 