
-- findMaxReducersA :: Stack -> [[Instruction]]
-- findMaxReducersA [] = [] 
-- findMaxReducersA [x] = [[]] 
-- findMaxReducersA [Just 0,Just 9,Just 4,Just 0] = [[Add,Mul,Sub],[Pop, Mul,Sub],[Add,Mul,Add],[Pop, Mul,Add]]
-- findMaxReducersA mybnums = output $ handler mybnums [] [] 0 0 
--   where handler [] (recent:acc) insAcc count splits = (insAcc, splits)
--         handler [x] (recent:r) insAcc count splits = (insAcc, splits)
--         handler (a:b:ns) acc insAcc count splits
--           | length (mostOfChainedIns a b) == 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionSingle  (map snd (mostOfChainedIns a b )) insAcc) (count + 1) (splits)
--           | length  (mostOfChainedIns a b) > 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionCombos (map snd (mostOfChainedIns a b )) insAcc) (count + length  (mostOfChainedIns a b)) (splits + 1)
--         output = (\(accum, splits) -> take (splits + 1) accum)

-- findMaxReducers_debug mybnums =  handler mybnums [] [] 0 0 
--   where handler [] (recent:acc) insAcc count splits = (recent,insAcc, "count -> " ++ show count , "splits -> " ++ show  splits)
--         handler [x] (recent:r) insAcc count splits = (recent,insAcc, "count -> " ++ show count, "splits -> " ++ show  splits)
--         handler (a:b:ns) acc insAcc count splits
--           | length (mostOfChainedIns a b) == 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionSingle  (map snd (mostOfChainedIns a b )) insAcc) (count + 1) (splits)
--           | length  (mostOfChainedIns a b) > 1 = handler ((head $ mostOfChained a b):ns) ((head $ mostOfChained a b):acc) ( instructionCombos (map snd (mostOfChainedIns a b )) insAcc) (count + length  (mostOfChainedIns a b)) (splits + 1)