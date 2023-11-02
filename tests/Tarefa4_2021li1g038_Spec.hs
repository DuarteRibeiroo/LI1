module Tarefa4_2021li1g038_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g038
import Tarefa4_2021li1g038
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    , "Tarefa 4 - Teste Move m1e1 Este" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    , "Tarefa 4 - Teste Move m1e1 Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    , "Tarefa 4 - Teste Move m1e1 InterageCaixa" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    , "Tarefa 4 - Teste colisão com bloco e caixa" ~: correrMovimentos j2 [AndarDireita,AndarDireita,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarDireita] ~=? Jogo m42 (Jogador (3,3) Este False)
    , "Tarefa 4 - Teste trepar caixa em direção oposta" ~: correrMovimentos j2 [Trepar] ~=? j2
    , "Tarefa 4 - Teste trepar e tentar andar para fora do mapa" ~: correrMovimentos j2 [AndarDireita,Trepar,AndarDireita,AndarDireita,AndarDireita] ~=? Jogo m42 (Jogador (6,3) Este False)
    , "Tarefa 4 - Teste pegar em caixa na direção contrária" ~: moveJogador j2 InterageCaixa ~=? j2
    , "Tarefa 4 - Teste pegar e largar em caixa" ~: correrMovimentos j2 [AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa] ~=? Jogo m42r (Jogador (3,3) Oeste False)
    , "Tarefa 4 - Teste completar mapa" ~: correrMovimentos j2 [AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,AndarEsquerda,AndarEsquerda] ~=? Jogo m42r (Jogador (0,3) Oeste False) 
    , "Tarefa 4 - Teste queda do jogador" ~: moveJogador j3 AndarEsquerda ~=? Jogo m43 (Jogador (6,3) Oeste False)
    , "Tarefa 4 - Teste queda de caixa" ~: moveJogador (Jogo m43 (Jogador (7,0) Oeste True)) InterageCaixa ~=? Jogo m43r (Jogador (7,0) Oeste False)
    , "Tarefa 4 - Teste mover jogador sem caixa por espaço com altura 1" ~: moveJogador (Jogo m43 (Jogador (3,4) Oeste False)) AndarEsquerda ~=? Jogo m43 (Jogador (2,4) Oeste False)
    , "Tarefa 4 - Teste mover jogador com caixa por espaço com altura 1" ~: correrMovimentos (Jogo m43 (Jogador (4,4) Este False)) [InterageCaixa,AndarEsquerda,AndarEsquerda] ~=? Jogo m43r5 (Jogador (3,4) Oeste True)
    , "Tarefa 4 - Teste trepar parede alta" ~: correrMovimentos j3 [AndarEsquerda,AndarDireita,Trepar] ~=? Jogo m43 (Jogador (6,3) Este False)
    , "Tarefa 4 - Teste trepar com caixa" ~: correrMovimentos j3 [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarDireita,InterageCaixa,AndarDireita,Trepar] ~=? Jogo m43r5 (Jogador (6,3) Este True)
    , "Tarefa 4 - Teste trepar com caixa em espaço de um bloco" ~: correrMovimentos (Jogo m43r2 (Jogador (4,4) Este False)) [InterageCaixa, AndarEsquerda,Trepar,Trepar] ~=? Jogo m43r3 (Jogador (3,3) Oeste True)
    , "Tarefa 4 - Teste pegar em caixa com bloco acima do jogador" ~: moveJogador (Jogo m43 (Jogador (2,4) Oeste False)) InterageCaixa ~=? Jogo m43 (Jogador (2,4) Oeste False)
    , "Tarefa 4 - Teste pegar em caixa com bloco acima da caixa" ~: moveJogador (Jogo m43r4 (Jogador (3,4) Oeste False)) InterageCaixa ~=? Jogo m43r4 (Jogador (3,4) Oeste False)
    , "Tarefa 4 - Teste largar caixa com bloco à frente da caixa" ~: moveJogador (Jogo m43 (Jogador (3,4) Oeste True)) InterageCaixa ~=? Jogo m43 (Jogador (3,4) Oeste True)

    ]