module Tarefa3_2021li1g038_Spec where

import Test.HUnit
import Tarefa3_2021li1g038
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m2e1" ~: "   <\nCP  \nXX  \nXXXX" ~=? show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2" ~: "   C\n   <\nCP  \nXX  \nXXXX" ~=? show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m2e3" ~: "   >\nCP  \nXX  \nXXXX" ~=? show m2e3
    , "Tarefa 3 - Teste Imprime Jogo m2e4" ~: "   C\n   >\nCP  \nXX  \nXXXX" ~=? show m2e4
    , "Tarefa 3 - Teste Imprime 1º jogo exemplo das FAQs" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X              X\nX                CX\nX               CCX\nX XXX    <   XC XX \nX X X    X  XXXXX  \nX X XCC XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       " ~=? show m7e1
    , "Tarefa 3 - Teste Imprime 2º jogo exemplo das FAQs" ~: "X                  X\nX                  X\nX                  X\nX                  X\nX   X       X      X\nXP  X   X C X C >  X\nXXXXXXXXXXXXXXXXXXXX" ~=? show m13e1
    , "Tarefa 3 - Teste Imprime 3º jogo exemplo das FAQs" ~: "     XXX    XXXXXXXXXX \n XXXX   XXXX          X\nX                     X\nX                     X\nX                     X\nX     X               X\nX     X               X\nX     XCCCC           X\nXP   XXXXXXX<         X\nXX XXX     XX X      CX\n X X        X XX   CCCX\n X X        X XX  CCCCX\n XXX        X XXXXXXXXX\n            XXX        " ~=? show m8e1
    , "Tarefa 3 - Teste Imprime Jogo m9e1" ~: "X       \nX      X\nXP     X\nXXXXX CX\nX   XC<X\n  XXXXXX\n X      " ~=? show m9e1 
    , "Tarefa 3 - Teste Imrpime Jogo com peças flutuantes abaixo do chão" ~: "      \n      \nP    >\nXXXXXX\n      \nXX  XX\n XC  X\n  XX  \n      \n    C \n    X " ~=? show m10e1
    , "Tarefa 3 - Teste Construir mapa com uma linha vazia" ~: "X      \nX     X\n      <\nXP  C X\nXXXXXXX" ~=? show m14e1
    ]