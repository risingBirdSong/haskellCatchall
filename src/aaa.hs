grida = [[4,3,2,-1],[3,2,1,-1],[1,1,-1,-2],[-1,-1,-2,-3]]

testa = [7,6,5,4,3,2,1,-1]

filternegs lst = filter (\x -> x < 0) lst
solve mtx = length (concat (map (filternegs) mtx))

practice =  filter (>5) [1,2,3,4,5,6,7,8]

