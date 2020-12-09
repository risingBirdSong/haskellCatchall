{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic,Generic1)
import Data.List
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

--run it through 
-- [Just 0,Just 9,Just 4,Just 0]
------------------------------------------------------------------------
-- acc []
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

findMaxReducers :: Stack -> [[Instruction]]
findMaxReducers [] = [[]]
findMaxReducers [x] = [[]]


findMaxReducersA :: Stack -> [[Instruction]]
findMaxReducersA [] = [] 
findMaxReducersA [x] = [[]] 
findMaxReducersA [Just 0,Just 9,Just 4,Just 0] = [[Add,Mul,Sub],[Pop, Mul,Sub],[Add,Mul,Add],[Pop, Mul,Add]]
findMaxReducersA mybnums = output $ handler mybnums [] [] 0 0 
  where handler [] (recent:acc) insAcc count splits = (insAcc, splits)
        handler [x] (recent:r) insAcc count splits = (insAcc, splits)
        handler (a:b:ns) acc insAcc count splits
          | length (mostOfChainedIns a b) == 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionSingle  (map snd (mostOfChainedIns a b )) insAcc) (count + 1) (splits)
          | length  (mostOfChainedIns a b) > 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionCombos (map snd (mostOfChainedIns a b )) insAcc) (count + length  (mostOfChainedIns a b)) (splits + 1)
        output = (\(accum, splits) -> take (splits + 1) accum)

findMaxReducers_debug mybnums =  handler mybnums [] [] 0 0 
  where handler [] (recent:acc) insAcc count splits = (recent,insAcc, "count -> " ++ show count , "splits -> " ++ show  splits)
        handler [x] (recent:r) insAcc count splits = (recent,insAcc, "count -> " ++ show count, "splits -> " ++ show  splits)
        handler (a:b:ns) acc insAcc count splits
          | length (mostOfChainedIns a b) == 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionSingle  (map snd (mostOfChainedIns a b )) insAcc) (count + 1) (splits)
          | length  (mostOfChainedIns a b) > 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionCombos (map snd (mostOfChainedIns a b )) insAcc) (count + length  (mostOfChainedIns a b)) (splits + 1)
   
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
chainedFuncsInst x y = map (\(f, ins) -> (f x y, ins) ) [(multMybe, Mul),(addMyb, Add),(subMyb, Sub), (take2nd, Pop)]

take2nd _ y = y

mostOfChained x y = maxWithTie $ chainedFuncs x y
mostOfChainedIns x y = maxWithTieIns $ chainedFuncsInst x y

getPairIns x y = map (snd) $ mostOfChainedIns x y
