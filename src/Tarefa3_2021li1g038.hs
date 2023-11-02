{- |
Module      : Tarefa3_2021li1g038
Description : Representação textual do jogo
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g038 where

import LI12122

{- |
Função que transforma o tipo de dados "Jogo" numa instância da class "Show".
-}
instance Show Jogo where
  show (Jogo m (Jogador (c,l) eo False)) = printJogoNCaixa m (c,l) eo 
  show (Jogo m (Jogador (c,l) eo True)) = printJogoCaixa m (c,l) eo 

{- |
Função que escreve a representação textual do jogo, caso a personagem não carregue uma
caixa, através do uso das funções "printPersonagem", "printNormal", e do uso recursivo
da função "printLinhaNormal".
-}
printJogoNCaixa :: [[Peca]] -> Coordenadas -> Direcao -> [Char]
printJogoNCaixa [] _ _ = []
printJogoNCaixa (h:t) (c,l) eo 
    |l==0 = printPersonagem h c eo ++ "\n" ++ printNormal t
    |otherwise = printLinhaNormal h ++ "\n" ++ printJogoNCaixa t (c, (l-1)) eo 

{- |
Função que escreve a representação textual do jogo caso a personagem carregue uma
caixa. No caso de a personagem se encontrar a primeira linha do mapa aplica as funções
"desenha1Linha" e "printJogo", caso contrário aplica a função "printJogoComCaixa".
-}
printJogoCaixa :: [[Peca]] -> Coordenadas -> Direcao -> [Char]
printJogoCaixa (h:t) (c,0) eo = desenha1linha c ++ "\n" ++ printJogoNCaixa (h:t) (c,0) eo
printJogoCaixa (h:t) (c,l) eo = printJogoComCaixa (h:t) (c,l) eo

{- |
Função que escreve a representação textual do jogo caso a personagem não se encontre
na primeira linha do mapa e carregue uma caixa, através da aplicação das funções 
"printPersonagem", "printNormal", "printCaixa", e do uso recursivo da função 
"printLinhaNormal".
-}
printJogoComCaixa :: [[Peca]] -> Coordenadas -> Direcao -> [Char]
printJogoComCaixa (h:t) (c,l) eo
  |l==0 = printPersonagem h c eo ++ "\n" ++ printNormal t
  |l==1 = printCaixa h c ++ "\n" ++ printJogoComCaixa t (c, 0) eo
  |otherwise = printLinhaNormal h ++ "\n" ++ printJogoComCaixa t (c, (l-1)) eo

{- |
Função que escreve a representação textual de todas as linhas após a linha onde se 
encontra a personagem, através do uso recursivo da função "printLinhaNormal".
-}
printNormal :: [[Peca]] -> [Char]
printNormal [] = []
printNormal [t] = printLinhaNormal t
printNormal (h:t) = printLinhaNormal h ++ "\n" ++ printNormal t

{- |
Função que escreve a representação textual da linha que contém a personagem tendo em 
conta a direção da personagem, através do uso da função "printLinhaNormal", e do uso
recursivo da função "printPeca".
-}
printPersonagem :: [Peca] -> Int -> Direcao -> [Char]
printPersonagem (h:t) 0 Este = ">" ++ printLinhaNormal t
printPersonagem (h:t) 0 Oeste = "<" ++ printLinhaNormal t
printPersonagem (h:t) a d = printPeca h ++ printPersonagem t (a-1) d

{- |
Função que escreve a representação textual da linha acima da personagem e que contém a
caixa que a personagem carrega, caso a personagem não se encontre na primeira linha, 
através do uso recursivo da função "printPeca".
-}
printCaixa :: [Peca] -> Int -> [Char]
printCaixa (h:t) c
  |c==0 = "C" ++ printLinhaNormal t
  |otherwise = printPeca h ++ printCaixa t (c-1)

{- |
Função que escreve a representação textual de uma linha normal, sem a personagem e sem
a necessidade de adicionar a caixa que a personagem eventualmente carregue consigo, de
peças, através da aplicação da função "printPeça" de forma recursiva.
-}
printLinhaNormal :: [Peca] -> [Char]
printLinhaNormal [] = ""
printLinhaNormal (h:t) = printPeca h ++ printLinhaNormal t

{- |
Função que converte uma peça na sua representação textual.
-}
printPeca :: Peca -> [Char]
printPeca Porta = "P"
printPeca Caixa = "C"
printPeca Bloco = "X"
printPeca Vazio = " "

{- |
Função que caso a personagem se encontre na primeira linha e carregue uma caixa cria
uma nova linha, acima da personagem, que contenha a caixa que a personagem carrega e
escreve a representação textual dessa linha. 
-}
desenha1linha :: Int -> [Char]
desenha1linha n
  |n==0 = "C"
  |otherwise = " " ++ desenha1linha (n-1)



