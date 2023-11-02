module Fixtures where

import LI12122


m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m2= [(Porta,(1,1)),(Caixa,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]

m3 = [(Porta,(1,1)),(Caixa,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,2)),(Bloco,(3,2)),(Bloco,(4,3)),(Bloco,(5,3))]

m4 = [(Porta,(1,1)),(Caixa,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(0,3)),(Bloco,(1,1)),(Bloco,(2,3)),(Bloco,(3,3))]

m5 = [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(3,3)),(Bloco,(3,2)),(Bloco,(4,1)),(Bloco,(4,2)),(Bloco,(4,3)),(Bloco,(4,4)),(Bloco,(5,4)),(Bloco,(6,3)),(Bloco,(6,4)),(Bloco,(7,1)),(Bloco,(7,2)),(Bloco,(7,3)),(Bloco,(8,1)),(Porta,(6,2))]

m6 = [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),(Bloco,(3,0)),(Bloco,(4,0)),(Bloco,(5,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Porta,(1,2)),(Bloco,(5,1)),(Bloco,(5,2)),(Bloco,(5,3)),(Bloco,(4,3)),(Caixa,(4,2))]

m7 = [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(0,10)),(Bloco,(1,0)),(Bloco,(1,1)),(Porta,(1,9)),(Bloco,(1,10)),(Bloco,(2,2)),(Bloco,(2,6)),(Bloco,(2,7)),(Bloco,(2,8)),(Bloco,(2,9)),(Bloco,(2,10)),(Bloco,(3,3)),(Bloco,(3,6)),(Bloco,(4,2)),(Bloco,(4,6)),(Bloco,(4,7)),(Bloco,(4,8)),(Bloco,(4,9)),(Bloco,(4,10)),(Bloco,(5,1)),(Caixa,(5,8)),(Bloco,(5,9)),(Bloco,(5,10)),(Bloco,(6,1)),(Caixa,(6,8)),(Bloco,(6,9)),(Bloco,(7,1)),(Bloco,(7,9)),(Bloco,(8,1)),(Bloco,(8,8)),(Bloco,(8,9)),(Bloco,(9,1)),(Bloco,(9,7)),(Bloco,(9,8)),(Bloco,(9,9)),(Bloco,(9,10)),(Bloco,(10,1)),(Bloco,(10,10)),(Bloco,(11,1)),(Bloco,(11,9)),(Bloco,(11,10)),(Bloco,(12,1)),(Bloco,(12,7)),(Bloco,(12,8)),(Bloco,(12,9)),(Bloco,(13,1)),(Bloco,(13,6)),(Bloco,(13,7)),(Bloco,(14,1)),(Caixa,(14,6)),(Bloco,(14,7)),(Bloco,(15,1)),(Bloco,(15,7)),(Bloco,(16,1)),(Caixa,(16,5)),(Bloco,(16,6)),(Bloco,(16,7)),(Bloco,(17,1)),(Caixa,(17,4)),(Caixa,(17,5)),(Bloco,(17,6)),(Bloco,(18,2)),(Bloco,(18,3)),(Bloco,(18,4)),(Bloco,(18,5))]

m8 = [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Bloco,(0,7)),(Bloco,(0,8)),(Bloco,(0,9)),(Bloco,(1,1)),(Porta,(1,8)),(Bloco,(1,9)),(Bloco,(1,10)),(Bloco,(1,11)),(Bloco,(1,12)),(Bloco,(2,1)),(Bloco,(2,12)),(Bloco,(3,1)),(Bloco,(3,9)),(Bloco,(3,10)),(Bloco,(3,11)),(Bloco,(3,12)),(Bloco,(4,1)),(Bloco,(4,9)),(Bloco,(5,0)),(Bloco,(5,8)),(Bloco,(5,9)),(Bloco,(6,0)),(Bloco,(6,5)),(Bloco,(6,6)),(Bloco,(6,7)),(Bloco,(6,8)),(Bloco,(7,0)),(Caixa,(7,7)),(Bloco,(7,8)),(Bloco,(8,1)),(Caixa,(8,7)),(Bloco,(8,8)),(Bloco,(9,1)),(Caixa,(9,7)),(Bloco,(9,8)),(Bloco,(10,1)),(Caixa,(10,7)),(Bloco,(10,8)),(Bloco,(11,1)),(Bloco,(11,8)),(Bloco,(11,9)),(Bloco,(12,0)),(Bloco,(12,9)),(Bloco,(12,10)),(Bloco,(12,11)),(Bloco,(12,12)),(Bloco,(12,13)),(Bloco,(13,0)),(Bloco,(13,13)),(Bloco,(14,0)),(Bloco,(14,9)),(Bloco,(14,10)),(Bloco,(14,11)),(Bloco,(14,12)),(Bloco,(14,13)),(Bloco,(15,0)),(Bloco,(15,10)),(Bloco,(15,11)),(Bloco,(15,12)),(Bloco,(16,0)),(Bloco,(16,12)),(Bloco,(17,0)),(Bloco,(17,12)),(Bloco,(18,0)),(Caixa,(18,11)),(Bloco,(18,12)),(Bloco,(19,0)),(Caixa,(19,10)),(Caixa,(19,11)),(Bloco,(19,12)),(Bloco,(20,0)),(Caixa,(20,10)),(Caixa,(20,11)),(Bloco,(20,12)),(Bloco,(21,0)),(Caixa,(21,9)),(Caixa,(21,10)),(Caixa,(21,11)),(Bloco,(21,12)),(Bloco,(22,1)),(Bloco,(22,2)),(Bloco,(22,3)),(Bloco,(22,4)),(Bloco,(22,5)),(Bloco,(22,6)),(Bloco,(22,7)),(Bloco,(22,8)),(Bloco,(22,9)),(Bloco,(22,10)),(Bloco,(22,11)),(Bloco,(22,12))]

m9 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Porta,(1,2)),(Bloco,(1,3)),(Bloco,(1,6)),(Bloco,(2,3)),(Bloco,(2,5)),(Bloco,(3,3)),(Bloco,(3,5)),(Bloco,(4,3)),(Bloco,(4,4)),(Bloco,(4,5)),(Caixa,(5,4)),(Bloco,(5,5)),(Bloco,(6,5)),(Bloco,(7,1)),(Bloco,(7,2)),(Bloco,(7,3)),(Bloco,(7,4)),(Bloco,(7,5))]

m10 = [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco,(0,5)),(Bloco,(1,5)),(Bloco,(1,6)),(Caixa,(2,6)),(Bloco,(2,7)),(Bloco,(3,7)),(Bloco,(5,5)),(Caixa,(4,9)),(Bloco,(4,10)),(Bloco,(4,5)),(Bloco,(5,6))]

m11 = [(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4)),(Porta,(4,3)),(Caixa,(0,3)),(Caixa,(0,1)),(Caixa,(1,2)),(Bloco,(1,3))]

m12 = [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(4,3)),(Vazio,(4,0))]

m13 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),(Bloco,(0,6)),(Porta,(1,5)),(Bloco,(1,6)),(Bloco,(2,6)),(Bloco,(3,6)),(Bloco,(4,4)),(Bloco,(4,5)),(Bloco,(4,6)),(Bloco,(5,6)),(Bloco,(6,6)),(Bloco,(7,6)),(Bloco,(8,5)),(Bloco,(8,6)),(Bloco,(9,6)),(Caixa,(10,5)),(Bloco,(10,6)),(Bloco,(11,6)),(Bloco,(12,4)),(Bloco,(12,5)),(Bloco,(12,6)),(Bloco,(13,6)),(Caixa,(14,5)),(Bloco,(14,6)),(Bloco,(15,6)),(Bloco,(16,6)),(Bloco,(17,6)),(Bloco,(18,6)),(Bloco,(19,0)),(Bloco,(19,1)),(Bloco,(19,2)),(Bloco,(19,3)),(Bloco,(19,4)),(Bloco,(19,5)),(Bloco,(19,6))]

m14 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 1)), (Porta, (1,3))]

m1r,m2r,m7r,m8r,m9r,m10r,m13r,m14r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r = [[Vazio,Vazio,Vazio,Vazio],[Caixa,Porta,Vazio,Vazio],[Bloco,Bloco,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]]

m7r = [[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],[Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]

m8r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Caixa,Bloco],[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]

m9r = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco],[Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]

m10r = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Bloco,Bloco,Vazio,Vazio,Bloco,Bloco],[Vazio,Bloco,Caixa,Vazio,Vazio,Bloco],[Vazio,Vazio,Bloco,Bloco,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Caixa,Vazio],[Vazio,Vazio,Vazio,Vazio,Bloco,Vazio]]

m13r = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

m14r = [[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Bloco, Porta, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (3, 0) Oeste False)

m2e2 :: Jogo
m2e2 = Jogo m2r (Jogador (3, 0) Oeste True)

m2e3 :: Jogo
m2e3 = Jogo m2r (Jogador (3, 0) Este False)

m2e4 :: Jogo
m2e4 = Jogo m2r (Jogador (3, 0) Este True)

m7e1 :: Jogo
m7e1 = Jogo m7r (Jogador (9, 6) Oeste False)

m8e1 :: Jogo
m8e1 = Jogo m8r (Jogador (12, 8) Oeste False)

m9e1 :: Jogo
m9e1 = Jogo m9r (Jogador (6, 4) Oeste True)

m10e1 :: Jogo
m10e1 = Jogo m10r (Jogador (5,2) Este False)

m13e1 :: Jogo
m13e1 = Jogo m13r (Jogador (16,5) Este False)

m14e1 :: Jogo
m14e1 = Jogo m14r (Jogador (6,2) Oeste False)

m42,m42r,m43,m43r,m43r2,m43r3,m43r4,m43r5 :: Mapa
m42 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Porta, Bloco, Vazio, Vazio, Vazio, Caixa, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m42r = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio],[Porta, Bloco, Caixa, Vazio, Vazio, Vazio, Vazio],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

m43 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Caixa, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m43r = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Caixa, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m43r2 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Caixa, Vazio, Caixa, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m43r3 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Caixa, Vazio, Caixa, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m43r4 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Caixa, Caixa, Vazio, Vazio, Caixa, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m43r5 = 
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

j2,j3 :: Jogo
j2 = Jogo m42 (Jogador (4,3) Oeste False)

j3 = Jogo m43 (Jogador (7,0) Oeste False)
