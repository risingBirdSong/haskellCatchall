
-- if the list we're checking is empty and the stack is empty, return True, it's a balanced set
-- also note, this syntax / style is called pattern matching, you can think of it as similar to if and else if... see how they are multiple definitions for balan? Each represents a different condition, or "pattern"
balan [] [] = True 
-- if there is one element left but an empty stack, return false, not balanced
balan [x] [] = False 
-- if the elements are empty, and we still have a stack, not balanced, return false
balan [] stk = False 
-- this is more pattern matching, q (for query) is the head of our elements, qs is all the rest
--stk is our stack
balan (q:qs) stk
-- note, the | (called guard) is another syntax that is similar to if and else if, it checks different conditions
-- if our query is '(', recurse onto the next query, but insert '(' onto the head of our stack to remember we encountered an opening
  | q == '(' = balan qs ('(':stk)
  -- if the stack is null, return false
  | null stk = False
  -- check if our parens are valid using the current query, an ')', and the head of the stack, if they are, recurse and drop the head of the stack since we've handled it. Note, recursing with qs, is also discarding q (which we want since we already handled it), because qs is everything but the query 
  | checkMatch q (head stk) = balan qs (drop 1 stk)
  -- they were valid!
    | otherwise = False 
  
-- checker just verifies that current closer closes the first opener we remembered 
checkMatch x y 
  | [x , y] == ")(" = True 
  | otherwise = False 