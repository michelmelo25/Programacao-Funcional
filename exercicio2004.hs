-- Questao 13 pagina 85
busca_sub_aux [] _ = True
busca_sub_aux _ [] = False
busca_sub_aux (x:y) (a:b) | a == x = True && busca_sub_aux y b
                          | otherwise = False

busca_sub :: String -> [String] -> [String]
busca_sub [] _ = []
busca_sub _ [] = []
busca_sub x (a:b) | busca_sub_aux x a == True = a : busca_sub x b
                  | otherwise = busca_sub x b

-- questao 17 pagina 85
comprime :: String -> String
comprime [] = []
comprime [a] = [a]
comprime (a:b:c) | a == b = '!' : a : comprime(b:c)
		 | otherwise = a : comprime(b:c)
