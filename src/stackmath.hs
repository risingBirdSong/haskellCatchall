{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic,Generic1)
import Data.List
import Data.List.Split
import Data.Ord

data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Show , Eq, Ord, Read, Generic)
type Stack = [Maybe Integer]
type SMProg = [Instruction]
-- may want to come back and clean this up, but happily heard back from Penelkata and this passes 100% of tests!
evalInst :: Stack -> SMProg -> Stack 
evalInst [] _ = error "empty stack!"
evalInst stack [] = stack  
evalInst (top:scnd:stack) (p:programs) 
  | p == Add = evalInst (((+) <$> top <*> scnd) : stack ) programs 
  | p == Sub = evalInst (((-) <$> top <*> scnd) : stack ) programs
  | p == Mul = evalInst (((*) <$> top <*> scnd) : stack) programs
  | p == Div && scnd == Just 0 = evalInst (Nothing : stack) programs
  | p == Div = evalInst ((div <$> top <*> scnd) : stack ) programs
  | p == Pop = evalInst (scnd:stack) programs
  | p == Dup = evalInst (top:top:scnd:stack) programs
evalInst (top:stack) (p:programs)
  | p == Dup = evalInst (top:top:stack) programs
  | p == Pop = evalInst stack programs
  | p == Add || p == Sub || p == Mul || p == Div = error "doesnt make sense!"

myDiv :: Maybe Int
myDiv = div <$> Just 30 <*> Just 3


findReducerA vals = reverse $ go vals []
  where go [x] acc = acc 
        go [] acc = acc
        go (a:b:ls) acc 
          | a > (Just 1) && b > (Just 1) = go ((multMybe a b):ls) (Mul:acc)
          | a == (Just 0) = go ((addMyb a b):ls) (Add:acc)
          | a == (Just 1) || b == (Just 1) = go ((addMyb a b):ls) (Add:acc)
          | a >= (Just 0) && b < (Just 0) = go ((subMyb a b):ls) (Sub:acc)
          | a < (Just 0) && a >= b = go ((subMyb a b):ls) (Sub:acc)
          | a < (Just 0) && a < b = go (b:ls) (Pop:acc)

findReducerB vals = reverse $ go vals []
  where go [x] acc = acc 
        go [] acc = acc
        go (a:b:ls) acc 
          | a > (Just 1) && b > (Just 1) = go ((multMybe a b):ls) (Mul:acc)
          | a == (Just 0) =  go (b:ls) (Pop:acc)
          | a == (Just 1) || b == (Just 1) = go (addMyb a b:ls) (Add:acc)
          | a >= (Just 0) && b < (Just 0) = go (subMyb a b:ls) (Sub:acc)
          | a < (Just 0) && a >= b = go (subMyb a b :ls) (Sub:acc)
          | a < (Just 0) && a < b = go (b:ls) (Pop:acc)
-- -5 -1 -> pop, because its greater than subtraction
-- -1 -4 -> sub, because its greater than two pops

multMybe a b = ((*) <$> a <*> b )
addMyb a b = ((+) <$> a <*> b)
subMyb a b = ((-) <$> a <*> b)

example1 = [Just 0, Just 1, Just 4, Just 5]
example1expected = [[Add,Add,Mul],[Pop,Add,Mul]]
example2 = [Just 10, Just (-1), Just 4, Just 5]
-- HEADS UP!! , hey expected the stack to be 44, I got 220, and that makes more sense to me!  
example2expected = [Sub, Mul, Mul]
example3 = [Just 1, Just 2, Just 3, Just 4, Just 5]
example3expected = [Add,Mul,Mul,Mul]

--run it through A
-- [Just 0,Just 9,Just 4,Just 0]
-- acc []
------------------------------------------------------------------------
-- encounter 1 -> 0 9
-- ins -> [Add, Pop] 
--since the accumulator is empty, split the instructions into sublists
-- acc -> [[Add],[Pop]]
-- stack -> [9,4,0]
------------------------------------------------------------------------
-- encounter 2 -> 9 4
-- ins -> [Mul]
-- the accumulator has two sublists, we want to add Mul to the end of all the sublists
-- acc -> [[Add,Mul],[Pop, Mul]]
-- stack -> [36,0]
------------------------------------------------------------------------
-- encounter 3 -> 36 0
-- ins -> [Sub,Add]
-- since the accumulator already has sublists, and we have more than one instruction (2) we must duplicate the accumulated sublists, each sublist must get each new ins, while keeping the instructions separate, (meaning in this case that Sub, and Add wont be added to the same sublist at the same time)
-- acc ->  [[Add,Mul,Sub],[Pop, Mul,Sub],[Add,Mul,Add],[Pop, Mul,Add]]
-- stack -> [36]
------------------------------------------------------------------------

--run it through B
-- [Just 0, Just 1, Just 4, Just 5]
-- acc -> []
------------------------------------------------------------------------
-- encounter 1 -> 0 1
-- ins -> [Pop, Add]
-- length of ins is > 1, therefore split the ins, there are no sublists to duplicate, so insert both into the acc
-- acc -> [[Pop], [Add]]
-- stack -> [1,4,5] 
------------------------------------------------------------------------
-- encounter 2 -> 1 4
-- ins -> [Add]
-- length of ins is 1, the single instruction should be appended to each sublist
-- acc -> [[Pop, Add], [Add,Add]]
-- stack -> [5,5]
------------------------------------------------------------------------
-- encounter 3 -> 5 5
-- ins -> [Mul]
-- single instruction, and the accum has sublists, therefore append Mul to each sublist
-- acc -> [[Pop, Add, Mul], [Add,Add, Mul]]
-- stack [25]
-- Checks Out 😌 (though order is different)
------------------------------------------------------------------------

--run it through C
-- [Just 10, Just (-1), Just 4, Just 5]
-- acc -> []
------------------------------------------------------------------------
-- encounter 1 -> 10 (-1)
-- ins -> [Sub]
-- the accumulator is empty, there for add the single instruction in
-- acc -> [Sub]
-- stack --> [11,4,5]
------------------------------------------------------------------------
-- encounter 2 -> 11 4
-- ins -> [Mul]
-- the accumulator is not split, therefore append Mul to the accum
-- acc -> [Sub, Mul]
-- stack --> [44,5]
------------------------------------------------------------------------
--encounter 3 -> 44 5
-- ins -> [Mul]
-- the accumulator is flat, therefore append the flat instruction to the end
-- acc -> [Sub, Mul, Mul]
--stack --> [220]
------------------------------------------------------------------------


example4 = [Just 0,Just 9,Just 4,Just 0]
example4first = [[Add],[Pop]]
example4Middle = [[Mul]]  
example4HypothesisA = [[Add, Mul, Add],[Pop, Mul, Sub]]
example4HypothesisB = [[Add, Mul],[Pop, Mul],[Add], [Sub]]
example4HypothesisC = [[Add, Mul,Add],[Add, Mul,Sub],[Pop, Mul,Add],[Pop, Mul,Sub]]
example4WORKS = [[Add, Mul,Add],[Add, Mul,Sub],[Pop, Mul,Add],[Pop, Mul,Sub]]
example4doesMixedWork = [[Add,Mul,Sub],[Pop, Mul,Sub],[Add,Mul,Add],[Pop, Mul,Add]]
example4HypothesisOrderMixed = [[Add, Mul,Sub],[Add, Mul,Add],[Pop, Mul,Add],[Pop, Mul,Sub]]

example4End = [[Add],[Sub]]
example5 =  [Just (-1),Just 5,Just (-7),Just 1,Just (-3),Just 8,Just 1]
example5expected = [Pop, Sub, Add, Sub, Mul, Add ]
                        -- 12 , 13, 16, 128, 129
example6 = [Just 0,Just 0,Just 1]
example7 = [Just 0,Just 0,Just 0,Just 0,Just 0,Just 1]
example8 = [Just 0,Just 0,Just 0,Just 0,Just 0]
-- experimenting with how to get 978 combos out of example 7 ...
expr1 = subsequences example7
expr2 =  map length expr1
instructions1 = [Add,Add,Mul]

-- part two 
specializedoutput tup = (\(x , y) -> (x, reverse y)) tup

-- findMaxReducers :: Stack -> [[Instruction]]
-- findMaxReducers [] = [] 
-- findMaxReducers [x] = [[]] 
-- findMaxReducers [Just 0, Just 1, Just 4, Just 5] = [[Add,Add,Mul],[Pop,Add,Mul]]
-- findMaxReducers [Just 10, Just (-1), Just 4, Just 5] = [[Sub], [Mul], [Mul]]
-- findMaxReducers [Just 1, Just 2, Just 3, Just 4, Just 5] =  [Add,Mul,Mul,Mul]

-- findMaxReducers [] = specializedoutput $ handler mybnums [] [] 
-- findMaxReducers :: (Ord (f b), Applicative f, Num b) => [f b] -> [[Instruction]]

-- im realizing I need a thoughtful way to know whether the accumulator is flat or nest.. I think the simplest way is to have a flag, defaulted to flat, if we ever get an insruction longer than 1, then we change the flag to nested...

-- findMaxReducers :: Stack -> [[Instruction]]
-- findMaxReducers stack = handler stack []
--   where handler [] acc = acc 
--         handler [x] acc = acc 
--         handler (a:b:ls) acc 
--           | length (getPairIns a b) == 1 =  ((mostOfChained a b):ls) (:acc)

   
instructionCombos [] acc = acc
instructionCombos (i:ins) acc = instructionCombos (ins) ([i]:acc)

instructionSingle ins [] = [ins]  
instructionSingle ins (sub:subs) =  (sub ++ ins) : instructionSingle ins (subs)  

instructionSingle_exclusive ins acc = (ins:acc)  
-- instructionSingle_exclusive ins (sub:subs) =  (sub ++ ins) : instructionSingle_exclusive ins (subs)  

maxWithTie ls = head $ group $ sortBy (comparing Down)  ls
maxWithTieIns ls = head $ groupBy (\(a,_) (aa,_) -> a == aa ) $ sortBy (comparing Down)  ls
comparisonTestA = (Just 1) == (Just 2)
comparisonTestB = (Just 2) == (Just 2)
chainedFuncs x y = map (\f -> f x y) [multMybe,addMyb,subMyb, take2nd]
chainedFuncsInst :: (Applicative f, Num b) => f b -> f b -> [(f b, Instruction)]
chainedFuncsInst x y = map (\(f, ins) -> (f x y, ins) ) [(multMybe, Mul),(addMyb, Add),(subMyb, Sub), (take2nd, Pop)]

take2nd _ y = y

mostOfChained x y = head $ maxWithTie $ chainedFuncs x y
mostOfChainedIns x y = maxWithTieIns $ chainedFuncsInst x y

getPairIns x y = map (snd) $ mostOfChainedIns x y
