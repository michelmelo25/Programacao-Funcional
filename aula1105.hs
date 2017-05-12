import Data.Char
numero2 :: Char -> Char -> Int
numero2 a b | ord a < 48 || ord a > 57 = 0
            | ord b < 48 || ord b > 57 = 0
            | otherwise = ((digitToInt a) * 10) + digitToInt b

type Nome = String
type Telefone = String
type Telefones [(Nome,Telefone)]

telefone :: Telefones -> Nome -> Telefone
telefone [] _ = "Telefone Desconhecido"
telefone (a:x) nome | nome == fst a = snd a
                    | otherwise = telefone x nome

novo_telefone :: Telefones -> Nome -> Telefone -> Telefones
novo_telefone [] b c = [(b,c)]
novo_telefone (a:x) b c | b == fst a = (b,c) : x
                        | otherwise = a : novo_telefone x b c
