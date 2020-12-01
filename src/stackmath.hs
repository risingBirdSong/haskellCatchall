data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Show , Eq, Ord, Read)
type Stack = [Maybe Int]
type SMProg = [Instruction]

evalInst :: Stack -> SMProg -> Stack 
evalInst [] _ = [] 
-- evalInst [x] _ = [x] 
evalInst stack [] = stack 
evalInst (top:scnd:stack) (p:programs) 
  | top == Nothing || scnd == Nothing && p == Add || p == Sub || p == Mul || p == Div = evalInst (Nothing:stack) programs
  | top == Nothing && p == Dup = evalInst (top:top:scnd:stack) programs
  | top == Nothing && p == Pop = evalInst (scnd:stack) programs
  | p == Add = evalInst (((+) <$> top <*> scnd) : stack ) programs 
  | p == Sub = evalInst (((-) <$> top <*> scnd) : stack ) programs
  | p == Mul = evalInst (((*) <$> top <*> scnd) : stack) programs
  | p == Div = evalInst ((div <$> top <*> scnd) : stack ) programs
  | p == Dup = evalInst (top:top:scnd:stack) programs
  | p == Pop = evalInst (scnd:stack) programs
evalInst (top:stack) (p:programs)
  | p == Dup = evalInst (top:top:stack) programs
  | p == Pop = evalInst stack programs

myStack :: Stack
myStack = [Just 1, Just 2, Just 3,Just 5]
myStackA :: Stack
myStackA = [Just 60, Just 2, Just 10, Just 3]

myIns :: SMProg
myIns = [Pop]

myDiv :: Maybe Int
myDiv = div <$> Just 30 <*> Just 3