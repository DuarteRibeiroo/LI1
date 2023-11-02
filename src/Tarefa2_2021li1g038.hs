{- |
Module      : Tarefa2_2021li1g038
Description : Construção/Desconstrução do mapa
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g038 where

import LI12122

{- |
Função que dada uma lista de pares, compostos por uma peça e as suas
coordenadas no mapa, a transforma no Mapa, através da aplicação da função
"juntaLinhas# à lista de pares, a esta função é também fornecida a informação
de que deve iniciar na coluna 0 e na linha 0.
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pecas = juntaLinhas 0 0 pecas 

{- |
Função que dado um mapa, o converte numa lista de pares, compostos por uma
peça e as suas coordenadas no mapa, através da aplicação da função "desconstruir"
ao mapa, a esta função é também fornecida a informação de que deve iniciar na 
linha 0.
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstruir 0 mapa

{- |
Função que recebe uma lista de pares, compostos por uma peça e as suas
coordenadas no mapa, a coluna e a linha a partir da qual deve funcionar,
e os transforma numa lista de listas, em que cada lista é composta por
todas as peças de uma dada linha, e em que as listas estão ordenadas 
de forma crescente pela linha que representam, através da execução, de 
forma recursiva, da função "escreveLinha" para todas as linhas.
-}
juntaLinhas :: Int -> Int -> [(Peca, Coordenadas)] -> [[Peca]]
juntaLinhas c l pecas
        |l > nLinhas pecas = []
        |otherwise = escreveLinha c l pecas : juntaLinhas c (l+1) pecas

{- |
Função que dada uma lista ordenada de elementos de uma linha escreve
a peça correspondente a cada coordenada com recurso a duas funções: uma 
que ordena os elementos da linha de acordo com a sua posição, "ordenaALinha"
que os ordena pela coluna correspondente de forma crescente, e uma "escreveLinha_2"
que converte os pares da lista ordenada fornecida, numa lista de peças ordenadas
de acordo com a sua posição no mapa.
-}
escreveLinha ::  Int -> Int -> [(Peca, Coordenadas)] -> [Peca]
escreveLinha c l pecas = escreveLinha_2 c l pecas (ordenaALinha l pecas)
                                   
{- |
Função que dada uma lista ordenada de pares, compostos por uma peça e a coluna 
que essa peça ocupa numa dada linha, escreve todas as peças que compõem essa 
linha. Começando na coluna 0, fornecida acima, a função compara a coluna que
tem de preencher com a coluna do primeiro par e com o número de colunas que a 
linha deve ter, fornecido pela função "nColunas", decidindo se deve escrever 
vazio, ou a peça do primeiro par, ou se deve terminar a lista.
-}
escreveLinha_2 ::  Int -> Int -> [(Peca, Coordenadas)] -> [(Peca, Int)] -> [Peca]
escreveLinha_2 c l pecas []
        |c > nColunas pecas = []
        |otherwise = Vazio : escreveLinha_2 (c+1) l pecas []
escreveLinha_2 c l pecas ((a, c'):t)
    |c==c' = a: escreveLinha_2 (c+1) l pecas t
    |c > nColunas pecas = []
    |otherwise = Vazio : escreveLinha_2 (c+1) l pecas ((a,c'):t)

{- |
Função que dada uma lista contendo os elementos de uma linha ordena
esses elementos de acordo com a coluna que lhes correspondem, de forma crescente,
com auxílio de duas funções: uma que fornece a lista que de elementos da linha,
"ordenaPLinhas", e uma que insere o primeiro elemento dessa lista na restante 
lista já organizada, "ordenaALinha_2". 
-}
ordenaALinha :: Int -> [(Peca, Coordenadas)] -> [(Peca, Int)]
ordenaALinha l pecas = (ordenaALinha_2 (ordenaPLinhas l pecas))

{- |
Função que dada uma lista de pares, compostos por uma peça e a coluna 
que essa peça ocupa no mapa, os ordena recursivamente, pelas colunas 
correspondentes de forma crescente, com auxílio de uma outra função, 
"inserePeca", que insere um par na lista já ordenada de forma crescente,
de acordo com a coluna correspondente. 
-}
ordenaALinha_2 :: [(Peca, Int)] -> [(Peca, Int)]
ordenaALinha_2 [] = []
ordenaALinha_2 (h:t) = inserePar h (ordenaALinha_2 t)

{- |
Função que dado um par de peça e coluna, e uma lista de pares, compostos 
por uma peça e a coluna que essa peça ocupa no mapa, neste caso 
previamente ordenados pela coluna correspondente, insere o par na  lista,
de modo a formar uma lista de pares ordenados pelas colunas de forma
crescente.
-}
inserePar :: (Peca, Int) -> [(Peca, Int)] -> [(Peca, Int)]
inserePar (a, c) [] = [(a, c)]
inserePar (a, c) ((a', c'):t)
    |c<c' = (a, c):(a', c'):t
    |otherwise = (a', c'): inserePar (a,c) t

{- |
Função que dada uma lista válida de pares, compostos por uma peça e 
as suas coordenadas no mapa, atribui a uma linha todas as peças que lhe 
pertencem.
-}
ordenaPLinhas :: Int -> [(Peca, Coordenadas)] -> [(Peca, Int)]
ordenaPLinhas l [] = []
ordenaPLinhas l ((a, (c', l')):t)
        |l==l' = ((a,c'): ordenaPLinhas l t)
        |otherwise = ordenaPLinhas l t

{- |
Função que dada uma lista válida de pares, compostos por uma peça e as
suas coordenadas no mapa, calcula o número de colunas
existentes no mapa que lhe corresponde.
-}
nColunas :: [(Peca, Coordenadas)] -> Int
nColunas ((a, (c, l)):[]) = c
nColunas ((a, (c, l)):(a', (c', l')):t)
        |c>=c' = nColunas ((a, (c, l)):t)
        |otherwise = nColunas ((a', (c', l')):t)

{- |
Função que dada uma lista válida de pares, compostos por uma peça e as
suas coordenadas no mapa, calcula o número de linhas existentes no mapa 
que lhe corresponde.
-}
nLinhas :: [(Peca, Coordenadas)] -> Int
nLinhas ((a, (c, l)):[]) = l
nLinhas ((a, (c, l)):(a', (c', l')):t)
        |l>=l' = nLinhas ((a, (c, l)):t)
        |otherwise = nLinhas ((a', (c', l')):t)

{- |
Função que aplica a função descontroiLinha a todas as linhas e forma uma única
lista resultante da junção de todas as listas, de pares, compostos por uma peça e 
a coordenada que esta ocupa no mapa, obtidas através da aplicação da referida 
função descontroiLinha.
-}
desconstruir :: Int -> [[Peca]] -> [(Peca, Coordenadas)]
desconstruir _ [] = []
desconstruir l (h:t) = desconstroiLinha 0 l h ++ desconstruir (l+1) t

{- |
Função que dada uma linha escreve produz uma lista de pares compostos por uma 
peça e a coordenada que este ocupa no mapa, com exceção das peças "Vazio", que
não serão apresentadas nessa lista.
-}
desconstroiLinha :: Int -> Int -> [Peca] -> [(Peca, Coordenadas)]
desconstroiLinha _ _ [] = []
desconstroiLinha c l (Caixa:xs) = (Caixa, (c, l)): desconstroiLinha (c+1) l xs
desconstroiLinha c l (Bloco:xs) = (Bloco, (c, l)): desconstroiLinha (c+1) l xs
desconstroiLinha c l (Porta:xs) = (Porta, (c, l)): desconstroiLinha (c+1) l xs
desconstroiLinha c l (Vazio:xs) = (Vazio, (c, l)): desconstroiLinha (c+1) l xs