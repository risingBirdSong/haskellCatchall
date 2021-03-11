import Data.List
import Data.List.Split
import qualified Data.Map as M
-- Singly-linked lists are already defined with this interface:
-- data ListNode a = ListNode { val :: a
--                            , next :: ListNode a
--                            } | Nil deriving Show
--
  
data ListNode a = ListNode { val :: a
                           , next :: ListNode a
                           } | Nil deriving Show

mylistA = (ListNode 1 (ListNode 2 (ListNode 3 (Nil))))
mylistB = (ListNode 4 (ListNode 5 (ListNode 6 (Nil))))
mylistC = mergeTwoLinkedLists mylistA mylistB

check (ListNode val next) = (val)


converttolst Nil = []
converttolst (ListNode val next) = val : converttolst next 

putBackInList [] = Nil  
putBackInList (x:xs) = ListNode x (putBackInList xs) 

removeKFromList l k = putBackInList $ filter (/=k) (converttolst l )

isListPalindrome l = tolist == (reverse tolist)
  where tolist =  converttolst l

mergeTwoLinkedLists l1 l2 = putBackInList . sort $ (converttolst l1) ++ (converttolst l2)

reverseNodesInKGroups l k = putBackInList 
            $ concatMap (\sub -> if length sub == k then reverse sub else sub)
             $ chunksOf k $ converttolst l


rearrangeLastN l n = putBackInList flipped
  where tolist = converttolst l
        (frst,scnd) = splitAt ((length tolist) - n) tolist
        flipped = scnd ++ frst

