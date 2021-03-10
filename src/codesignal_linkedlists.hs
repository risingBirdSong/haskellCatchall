-- Singly-linked lists are already defined with this interface:
-- data ListNode a = ListNode { val :: a
--                            , next :: ListNode a
--                            } | Nil deriving Show
--
  
data ListNode a = ListNode { val :: a
                           , next :: ListNode a
                           } | Nil deriving Show

mylistA = (ListNode 3 (ListNode 4 (ListNode 3 (Nil))))

check (ListNode val next) = (val)


converttolst Nil = []
converttolst (ListNode val next) = val : converttolst next 

putBackInList [] = Nil  
putBackInList (x:xs) = ListNode x (putBackInList xs) 

removeKFromList l k = putBackInList $ filter (/=k) (converttolst l )

isListPalindrome l = tolist == (reverse tolist)
  where tolist =  converttolst l

