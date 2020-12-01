data Instruction = Add | Sub | Mul | Div | Dup | Pop deriving (Show , Eq, Ord, Read)
type Stack = [Maybe Int]
type SMProg = [Instruction]
evalInst :: Stack -> SMProg -> Stack
evalInst [] _ = []
evalInst stack [] = stack
evalInst (top:stack) (Dup:ps) = evalInst (top:top:stack) ps
evalInst (_:stack) (Pop:ps) = evalInst stack ps
evalInst (top:stack) (_:ps) = (top:stack)
evalInst (top:scnd:stack) (Add:ps) = evalInst (((+) <$> top <*> scnd) :stack)  ps
evalInst (top:scnd:stack) (Sub:ps) = evalInst (((-) <$> top <*> scnd):stack)  ps
evalInst (top:scnd:stack) (Mul:ps) = evalInst (((*) <$> top <*> scnd):stack)  ps
evalInst ((Just 0):stack) (Div:ps) = evalInst (Nothing:stack)  ps
evalInst (top:scnd:stack) (Div:ps) = evalInst ((div <$> top <*> scnd):stack)  ps
