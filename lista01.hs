-- 1 Menor De Dois
menorDeDois x y | x < y = x
                | otherwise = y

-- 2 Menor de Tres
menorDeTres x y z | x < menorDeDois y z = x
                  | otherwise = menorDeDois y z
-- 3 Fatorial
fatorial 0 = 1
fatorial x = x * fatorial(x - 1)

-- 4 Fibonacci
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)

-- 5 Elemento
elemento [] _ = []
elemento (a:x) n | n == 0 = a
                 | otherwise = elemento x (n - 1)

-- 6 Pertence
pertence [] x = False
pertence (a:b) x | a == x = True
                 | otherwise = pertence b x
-- 7 Total
total [] = 0
total (a:b) = 1 + total b

-- 8 Maior
--maior [] = []
maior [a] = a
maior (a:b) | a > maior(b) = a
            | otherwise = maior b

-- 9 Frequencia
frequencia [] _ = 0
frequencia (a:b) x | a == x = 1 + frequencia b x
                   | otherwise = frequencia b x

-- 10 Unico
unico [] _ = False
unico x y | frequencia x y == 1 = True
          | otherwise = False

-- 11 Maiores Que
maioresQue [] _ = []
maioresQue (a:b) x | a > x = a : maioresQue b x
                   | otherwise = maioresQue b x 

-- 12 Concat
concatena :: [y] -> [y] -> [y]
concatena [] x = x
concatena x [] = x
concatena (a:b) x = a : concatena b x

-- 13 Calda
calda [] = []
calda (a:b) = b

-- 14 Corpo
corpo [] = []
corpo [y] = []
corpo (a:b) = a : corpo b

-- 15 Unique

-- 16 Menores 
--menores 
