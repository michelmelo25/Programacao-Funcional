--quetao01
inverte [] = []
inverte (x:y) = (inverte y) ++ [x]

palindrome [] [] = True
palindrome _ [] = False
palindrome [] _ = False
palindrome (a:b) (x:y) | a == x = True && palindrome b y
                       | otherwise = False

isPalindrome x = palindrome x (inverte x)

--questao02
taill [a] = a
taill (x:y) = taill y

corpo [a] = []
corpo (x:y) = x : corpo y 

rotDir n x | n < 1 = x
           | otherwise = rotDir (n - 1) ((taill x) : (corpo x))

--questao03
listaPar [] = []
listaPar (x:y) | mod x 2 == 0 = x : listaPar y
               | otherwise = listaPar y

listaImpar [] = []
listaImpar (x:y) | mod x 2 /= 0 = x : listaImpar y
                 | otherwise = listaImpar y

splitints x = (listaImpar x, listaPar x)

--questao04
isPrimo n | tamanho([x | x <-[1..n+1], mod n x == 0]) == 2 = True
          | otherwise = False
tamanho [] = 0
tamanho [a] = 1
tamanho (x:y) = 1 + tamanho y

--questao05
intercal [] x = x
intercal x [] = x
intercal (a:b) (x:y) = a : x : intercal b y
