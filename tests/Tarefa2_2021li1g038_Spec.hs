module Tarefa2_2021li1g038_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g038
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
--    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa [] - o mapa não é válido
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
--    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa [] - o mapa não é válido
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    , "Tarefa 2 - Teste Construir Sobrepor Peças" ~: constroiMapa [(Porta, (7, 4))] ~=?  constroiMapa [(Porta, (7, 4)), (Porta, (7, 4))]
    , "Tarefa 2 - Teste Construir Mapa m2" ~: constroiMapa m2 ~=? m2r
    , "Tarefa 2 - Teste Construir 1º mapa exemplo das FAQs" ~: constroiMapa m7 ~=? m7r
    , "Tarefa 2 - Teste Construir 2º mapa exemplo das FAQs" ~: constroiMapa m13 ~=? m13r
    , "Tarefa 2 - Teste Construir 3º mapa exemplo das FAQs" ~: constroiMapa m8 ~=? m8r
    , "Tarefa 2 - Teste Construir mapa com dois caminhos possíveis para chão diferentes" ~: constroiMapa m9 ~=? m9r 
    , "Tarefa 2 - Teste Construir mapa com peças flutuantes abaixo do chão" ~: constroiMapa m10 ~=? m10r
    , "Tarefa 2 - Teste Construir mapa com uma linha vazia" ~: constroiMapa m14 ~=? m14r
    , "Tarefa 2 - Teste Desconstruir Mapa m2" ~: sort (desconstroiMapa m2r) ~=? sort m2
    , "Tarefa 2 - Teste Desconstruir 1º mapa exemplo das FAQs" ~: sort (desconstroiMapa m7r) ~=? sort m7
    , "Tarefa 2 - Teste Desconstruir 2º mapa exemplo das FAQs" ~: sort (desconstroiMapa m13r) ~=? sort m13
    , "Tarefa 2 - Teste Desconstruir 3º mapa exemplo das FAQs" ~: sort (desconstroiMapa m8r) ~=? sort m8
    , "Tarefa 2 - Teste Desconstruir mapa com dois caminhos possíveis para chão diferentes" ~: sort (desconstroiMapa m9r) ~=? sort m9
    , "Tarefa 2 - Teste Desconstruir mapa com peças flutuantes abaixo do chão" ~: sort (desconstroiMapa m10r) ~=? sort m10
    , "Tarefa 2 - Teste Desconstruir mapa com uma linha vazia" ~: sort (desconstroiMapa m14r) ~=? sort m14
    , "Tarefa 2 - Teste Identidade m2" ~: sort m2 ~=? sort (desconstroiMapa(constroiMapa m2))
    , "Tarefa 2 - Teste Identidade m2r" ~: m2r ~=? constroiMapa(desconstroiMapa m2r)
    , "Tarefa 2 - Teste Identidade m7" ~: sort m7 ~=? sort (desconstroiMapa(constroiMapa m7))
    , "Tarefa 2 - Teste Identidade m7r" ~: m7r ~=? constroiMapa(desconstroiMapa m7r)
    , "Tarefa 2 - Teste Identidade m8" ~: sort m8 ~=? sort (desconstroiMapa(constroiMapa m8))
    , "Tarefa 2 - Teste Identidade m8r" ~: m8r ~=? constroiMapa(desconstroiMapa m8r)
    , "Tarefa 2 - Teste Identidade m9" ~: sort m9 ~=? sort (desconstroiMapa(constroiMapa m9))
    , "Tarefa 2 - Teste Identidade m9r" ~: m9r ~=? constroiMapa(desconstroiMapa m9r)
    , "Tarefa 2 - Teste Identidade m10" ~: sort m10 ~=? sort (desconstroiMapa(constroiMapa m10))
    , "Tarefa 2 - Teste Identidade m10r" ~: m10r ~=? constroiMapa(desconstroiMapa m10r)
    , "Tarefa 2 - Teste Identidade m13" ~: sort m13 ~=? sort (desconstroiMapa(constroiMapa m13))
    , "Tarefa 2 - Teste Identidade m13r" ~: m13r ~=? constroiMapa(desconstroiMapa m13r)
    , "Tarefa 2 - Teste Identidade m14" ~: sort m14 ~=? sort (desconstroiMapa(constroiMapa m14))
    , "Tarefa 2 - Teste Identidade m14r" ~: m14r ~=? constroiMapa(desconstroiMapa m14r)
    ]