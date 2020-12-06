data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Show , Eq, Ord, Read)
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
          | a > (Just 1) && b > (Just 1) = go (b:ls) (Mul:acc)
          | a == (Just 0) =  go (b:ls) (Pop:acc)
          | a == (Just 1) || b == (Just 1) = go (b:ls) (Add:acc)
          | a >= (Just 0) && b < (Just 0) = go (b:ls) (Sub:acc)
          | a < (Just 0) && a >= b = go (b:ls) (Sub:acc)
          | a < (Just 0) && a < b = go (b:ls) (Pop:acc)
-- -5 -1 -> pop, because its greater than subtraction
-- -1 -4 -> sub, because its greater than two pops

multMybe a b = ((*) <$> a <*> b )
addMyb :: (Applicative f, Num b) => f b -> f b -> f b
addMyb a b = ((+) <$> a <*> b)
subMyb a b = ((-) <$> a <*> b)

example1 = [Just 0, Just 1, Just 4, Just 5]
example2 = [Just 10, Just (-1), Just 4, Just 5]
ins1 = [Add,Add,Mul]
