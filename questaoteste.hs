comp [] = 0
comp [a] = 1
comp (x:y) = 1 + comp y
