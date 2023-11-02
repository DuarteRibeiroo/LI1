module Tarefa1_2021li1g038_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g038
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste valida Mapa sem portas" ~: validaPotencialMapa [(Bloco,(0,1)),(Bloco,(1,1))] ~=? False
    , "Tarefa 1 - Teste valida mapa sem chão contínuo" ~: validaPotencialMapa [(Porta,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(3,2))] ~=? False
    , "Tarefa 1 - Teste valida mapa com chão nivelado" ~: validaPotencialMapa m2 ~=? True
    , "Tarefa 1 - Teste valida mapa com chão contínuo desnivelado" ~: validaPotencialMapa m3 ~=? True
    , "Tarefa 1 - Teste valida mapa com chão descontínuo, mas peça flutuante no local de descontinuidade" ~: validaPotencialMapa m4 ~=? False
    , "Tarefa 1 - Teste valida mapa com descontínuo" ~: validaPotencialMapa m5 ~=? False
    , "Tarefa 1 - Teste valida mapa com 'teto' mas sem chão (Existe chão contínuo, mas encontra-se acima da porta)" ~: validaPotencialMapa m6 ~=? False
    , "Tarefa 1 - Teste valida 1º mapa exemplo das FAQs" ~: validaPotencialMapa m7 ~=? True
    , "Tarefa 1 - Teste valida 3º mapa exemplo das FAQs" ~: validaPotencialMapa m8 ~=? True
    , "Tarefa 1 - Teste valida mapa com dois caminhos possíveis para chão diferentes, um válido e outro inválido" ~: validaPotencialMapa m9 ~=? True
    , "Tarefa 1 - Teste valida mapa m9, mas com duas declarações de peça para as coordenadas (0,0)" ~: validaPotencialMapa (m9++[(Vazio,(0,0))]) ~=? False
    , "Tarefa 1 - Teste valida mapa válido, mas com peças flutuantes abaixo do chão" ~: validaPotencialMapa m10 ~=? True
    , "Tarefa 1 - Teste valida mapa sem espaços vazios" ~: validaPotencialMapa [(Porta,(0,0)),(Bloco,(0,1)),(Bloco,(1,1)),(Caixa,(1,0))] ~=? False
    , "Tarefa 1 - Teste valida mapa com caixas flutuantes" ~: validaPotencialMapa m11 ~=? False
    , "Tarefa 1 - Teste valida mapa com chão contínuo, exceto na última coluna, que é constituída apenas por blocos vazios" ~: validaPotencialMapa m12 ~=? False
    
    ]
