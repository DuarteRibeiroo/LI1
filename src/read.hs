module Read where

import LI12122
import Data.String

listamapas :: String -> [String] -> [(String,Jogo)]
listamapas _ [] = []
listamapas n ((x:xs):ls)
    |x == '-' = (n,Jogo (convertemapa [] xs) (Jogador (convertecoordenadax [] (eliminaate '(' xs),convertecoordenaday [] (eliminaate ',' xs)) (fst (convertertedircaixa xs)) (snd (convertertedircaixa xs)))) : (listamapas [] ls)
    |otherwise = listamapas (n ++ [x]) (xs:ls)

-- |Esta funçao elimina toda a string até certo caracter ser encontrado
eliminaate :: Char -> String -> String
eliminaate c (x:xs)
    |c == x = xs
    |otherwise =  eliminaate c xs

convertemapa :: [Peca] -> String -> [[Peca]]
convertemapa m (l:ls)
    |l == 'V' = convertemapa (m ++ [Vazio]) ls
    |l == 'B' = convertemapa (m ++ [Bloco]) ls
    |l == 'P' = convertemapa (m ++ [Porta]) ls
    |l == 'C' = convertemapa (m ++ [Caixa]) ls
    |l == '.' = m : (convertemapa [] ls)
    |otherwise = []

convertecoordenadax :: String -> String-> Int
convertecoordenadax n (x:xs)
    |x == ',' = read n :: Int
    |otherwise = convertecoordenadax (n ++ [x]) xs

convertecoordenaday :: String -> String-> Int
convertecoordenaday n (x:xs)
    |x == ')' = read n :: Int
    |otherwise = convertecoordenaday (n ++ [x]) xs

convertertedircaixa :: String -> (Direcao,Bool)
convertertedircaixa (x:y:z:xs)
    |x == '-' = case y of
                'E' -> if z == 'F' then (Este,False) else (Este,True)
                'O' -> if z == 'F' then (Oeste,False) else (Oeste,True)
    |otherwise = convertertedircaixa (y:z:xs)

mapasguardados :: IO String
mapasguardados = readFile "niveiscriados.txt"
                