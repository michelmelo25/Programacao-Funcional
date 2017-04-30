dAB xA yA xB yB | yA == yB = abs(xA - xB)
                | xA == xB = abs(yA - yB)
                | otherwise = sqrt((xA - xB)^2 + (yA - yB)^2)

iguais3 a b c | a == b && b == c = 3
              | a /= b && b /= c = 0
              | otherwise = 2

potencia2 n = n^2

xor a b = (a || b) && (not (b && a))

soma 1 = 1
soma n = n + soma(n - 1)

fatorial 0 = 1
fatorial n = n * fatorial(n - 1)

soma_e_sub :: (Int,Int) -> (Int,Int)
soma_e_sub (a,b) = (a + b, a - b)

type Seq_Caract = String
type Nomes = (Seq_Caract, Seq_Caract, Seq_Caract, Seq_Caract)

f_nomes_est :: Nomes
f_nomes_est = ("Inverno", "Outono", "Primavera", "Verao")

selec_inv (x,_,_,_) = x
selec_outono (_,x,_,_) = x
selec_prima (_,_,x,_) = x
selec_Verao (_,_,_,x) = x

-- Média das idades até o registro x
--soma_idade :: Float -> Float 
--soma_idade x | x == 1 idade (pessoa 1)
             | otherwise = idade (pessoa x) + (soma_idade (x - 1))

--media_idade :: Float -> Float
--media_idade x = (soma_idade x) / x

par :: Int -> Bool
par x = mod x 2 == 0

constroi_lista0 = [x * x | x <- [2..50], par x]

maior [a] = a
maior (a:x) | a > maior x = a
            |otherwise = maior x

comp [] = 0
comp (a:x) = 1 + comp x

maior_tupla_aux n (a:x) | a == maior (a:x) = n
                        | otherwise = maior_tupla_aux n

maior_tupla (a:x) = maior_tupla_aux 1 (a:x)
