{- |
Module      : Tarefa4_2021li1g038
Description : Movimentação do personagem
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g038 where

import LI12122


-- ==Funções Principais

-- |Esta função aplica um movimento dado um estado de jogo
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j AndarEsquerda = movesquerda j --Quando é feito um movimento à esquerda, irá ser usada a função para mover o jogador para a esquerda, se possível
moveJogador j AndarDireita = movedireita j--Quando é feito um movimento à direita, irá ser usada a função para mover o jogador para a direita, se possível
moveJogador j Trepar = posstrepar j --Quando o jogador tenta trepar um bloco a função que verifica se isso é possível é invocada
moveJogador j InterageCaixa = pegacaixa j -- Quando o jogador tenta interagir com uma caixa esta função é invocada que verifica se é possível fazer isso

-- |Esta função aplica uma cadeia de movimentos a um estado de jogo
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo -- Se não houverem mais movimentos para realizar, o jogo não se irá alterar
correrMovimentos jogo (m:ms) = correrMovimentos (moveJogador jogo m) ms -- Caso haja movimentos por realizar, realizar-se-á o primeiro movimento e depois serão realizados os próximos movimentos em função do estado de jogo gerado pelos movimentos anteriores


-- ==Funções auxiliares


-- ===AndarEsquerda
-- | Esta função aplica um movimento à esquerda ao estado de jogo
movesquerda :: Jogo -> Jogo
movesquerda (Jogo m (Jogador (x,y) _ False)) --Caso o jogador não esteja a segurar numa caixa
    |encontrapeca4 m (x-1,y) == Just Vazio || encontrapeca4 m (x-1,y) == Just Porta = (Jogo m (Jogador (blocoabaixo (x-1,y) m) Oeste False)) -- Se a peça à esquerda de um jogador for vazia ou uma porta, o jogador pode avançar para essas coordenadas (Tendo em conta que o jogador pode cair)
    |otherwise = (Jogo m (Jogador (x,y) Oeste False)) -- Caso contrário, o jogador não muda de posição, mas a sua direção irá mudar para Oeste
movesquerda (Jogo m (Jogador (x,y) _ True)) -- Caso o jogador esteja a segurar numa caixa
    |(encontrapeca4 m (x-1,y) == Just Vazio || encontrapeca4 m (x-1,y) == Just Porta) && (encontrapeca4 m (x-1,y-1) == Just Vazio || encontrapeca4 m (x-1,y-1) == Nothing)= (Jogo m (Jogador (blocoabaixo (x-1,y) m) Oeste True)) -- Se a peça à esquerda de um jogador for vazia ou uma porta, e a peça acima dessa for vazia, o jogador pode avançar para essas coordenadas (Tendo em conta que o jogador pode cair)
    |otherwise = (Jogo m (Jogador (x,y) Oeste True)) -- Caso contrário, o jogador não muda de posição, mas a sua direção irá mudar para Oeste


-- ===AndarDireita
-- | Esta função aplica um movimento à direita ao estado de jogo
movedireita :: Jogo -> Jogo
movedireita (Jogo m (Jogador (x,y) _ False)) --Caso o jogador não esteja a segurar numa caixa
    |encontrapeca4 m (x+1,y) == Just Vazio || encontrapeca4 m (x+1,y) == Just Porta = (Jogo m (Jogador (blocoabaixo (x+1,y) m) Este False)) -- Se a peça à direita de um jogador for vazia ou uma porta, o jogador pode avançar para essas coordenadas (Tendo em conta que o jogador pode cair)
    |otherwise = (Jogo m (Jogador (x,y) Este False)) -- Caso contrário, o jogador não muda de posição, mas a sua direção irá mudar para Este
movedireita (Jogo m (Jogador (x,y) _ True))
    |(encontrapeca4 m (x+1,y) == Just Vazio || encontrapeca4 m (x+1,y) == Just Porta) && (encontrapeca4 m (x+1,y-1) == Just Vazio || encontrapeca4 m (x+1,y-1) == Nothing) = (Jogo m (Jogador (blocoabaixo (x+1,y) m) Este True))-- Se a peça à direita de um jogador for vazia ou uma porta, e a peça acima dessa for vazia, o jogador pode avançar para essas coordenadas (Tendo em conta que o jogador pode cair)
    |otherwise = (Jogo m (Jogador (x,y) Este True))---- Caso contrário, o jogador não muda de posição, mas a sua direção irá mudar para Este


-- ===Trepar
-- |Esta função verifica se é possível trepar um bloco e, nesse caso, altera a posição do jogador para a nova posição
posstrepar :: Jogo -> Jogo
posstrepar (Jogo m (Jogador (x,y) Oeste False)) -- Caso em que o jogador tenta trepar um bloco à sua esquerda sem estar a segurar numa caixa
    |encontrapeca4 m (x-1,y) == Just Bloco ||encontrapeca4 m (x-1,y) == Just Caixa = if encontrapeca4 m (x-1,y-1) == Just Vazio || encontrapeca4 m (x-1,y-1) == Just Porta then (Jogo m (Jogador (x-1,y-1) Oeste False)) else (Jogo m (Jogador (x,y) Oeste False)) -- Se o bloco à sua esquerda for trepável (caixa ou bloco) e o jogador poder mover-se para as coordenadas acima desse, o jogador irá tomar essas coordenadas, caso contrário irá manter-se na mesma posição
    |otherwise = (Jogo m (Jogador (x,y) Oeste False))
posstrepar (Jogo m (Jogador (x,y) Oeste True)) -- Caso em que o jogador tenta trepar um bloco à sua esquerda enquanto segura numa caixa
    |encontrapeca4 m (x-1,y) == Just Bloco ||encontrapeca4 m (x-1,y) == Just Caixa = if (encontrapeca4 m (x-1,y-1) == Just Vazio || encontrapeca4 m (x-1,y-1) == Just Porta) && (encontrapeca4 m (x-1,y-2) == Just Vazio || encontrapeca4 m (x-1,y-2) == Nothing) then (Jogo m (Jogador (x-1,y-1) Oeste True)) else (Jogo m (Jogador (x,y) Oeste True))-- Se o bloco à sua esquerda for trepável (caixa ou bloco) e o jogador poder mover-se para as coordenadas acima desse e a peça acima de onde o jogador treparia for vazia, o jogador irá tomar essas coordenadas, caso contrário irá manter-se na mesma posição
    |otherwise = (Jogo m (Jogador (x,y) Oeste True))
posstrepar (Jogo m (Jogador (x,y) Este False))-- Caso em que o jogador tenta trepar um bloco à sua direita sem estar a segurar numa caixa
    |encontrapeca4 m (x+1,y) == Just Bloco ||encontrapeca4 m (x+1,y) == Just Caixa = if encontrapeca4 m (x+1,y-1) == Just Vazio || encontrapeca4 m (x+1,y-1) == Just Porta then (Jogo m (Jogador (x+1,y-1) Este False)) else (Jogo m (Jogador (x,y) Este False)) -- Se o bloco à sua direita for trepável (caixa ou bloco) e o jogador poder mover-se para as coordenadas acima desse, o jogador irá tomar essas coordenadas, caso contrário irá manter-se na mesma posição
    |otherwise = (Jogo m (Jogador (x,y) Este False))
posstrepar (Jogo m (Jogador (x,y) Este True))-- Caso em que o jogador tenta trepar um bloco à sua direita enquanto segura numa caixa
    |encontrapeca4 m (x+1,y) == Just Bloco ||encontrapeca4 m (x+1,y)== Just Caixa = if (encontrapeca4 m (x+1,y-1) == Just Vazio || encontrapeca4 m (x+1,y-1) == Just Porta)&& (encontrapeca4 m (x+1,y-2) == Just Vazio || encontrapeca4 m (x+1,y-2) == Nothing)  then (Jogo m (Jogador (x+1,y-1) Este True)) else (Jogo m (Jogador (x,y) Este True)) -- Se o bloco à sua direita for trepável (caixa ou bloco) e o jogador poder mover-se para as coordenadas acima desse e a peça acima de onde o jogador treparia for vazia, o jogador irá tomar essas coordenadas, caso contrário irá manter-se na mesma posição
    |otherwise = (Jogo m (Jogador (x,y) Este True))


-- ===InterageCaixa
-- |Esta função determina o estado do jogo quando o jogador tenta interagir com uma caixa
pegacaixa :: Jogo -> Jogo
pegacaixa (Jogo m (Jogador (x,y) Oeste False)) --Caso em que o jogador tenta pegar numa caixa à sua esquerda
    |encontrapeca4 m (x-1,y) == Just Caixa && (encontrapeca4 m (x-1,y-1) == Just Vazio || encontrapeca4 m (x-1,y-1) == Nothing) && (encontrapeca4 m (x,y-1) == Just Vazio || encontrapeca4 m (x,y-1) == Nothing) = (Jogo (alteramapa Vazio (x-1,y) m []) (Jogador (x,y) Oeste True)) -- Se o jogador tiver uma caixa à sua esquerda e não houver obstáculos acima do jogador e da caixa, o jogador pega na caixa e altera o mapa para remover a caixa do mapa
    |otherwise = (Jogo m (Jogador (x,y) Oeste False)) --Caso contrário, o jogo não se altera
pegacaixa (Jogo m (Jogador (x,y) Oeste True)) --Caso em que o jogador tenta pousar uma caixa à sua esquerda
    |(encontrapeca4 m (x-1,y-1) == Just Vazio || (encontrapeca4 m (x-1,y-1) == Nothing && encontrapeca4 m (x-1,y) /= Nothing)) && blocoabaixo' (x-1,y-1) m  = Jogo (alteramapa Caixa (blocoabaixo (x-1,y-1) m) m []) (Jogador (x,y) Oeste False)-- Se a peça à esquerda da caixa for Vazia, a caixa é largada acima do primeiro bloco abaixo da caixa nessa coluna 
    |otherwise = (Jogo m (Jogador (x,y) Oeste True)) --Caso contrário, o jogo não se altera
pegacaixa (Jogo m (Jogador (x,y) Este False))--Caso em que o jogador tenta pegar numa caixa à sua direita
    |encontrapeca4 m (x+1,y) == Just Caixa && (encontrapeca4 m (x+1,y -1) == Just Vazio || encontrapeca4 m (x+1,y-1) == Nothing) && (encontrapeca4 m (x,y-1) == Just Vazio || encontrapeca4 m (x,y-1) == Nothing) = (Jogo (alteramapa Vazio (x+1,y) m []) (Jogador (x,y) Este True)) -- Se o jogador tiver uma caixa à sua direita e não houver obstáculos acima do jogador e da caixa, o jogador pega na caixa e altera o mapa para remover a caixa do mapa
    |otherwise = (Jogo m (Jogador (x,y) Este False)) --Caso contrário, o jogo não se altera
pegacaixa (Jogo m (Jogador (x,y) Este True)) --Caso em que o jogador tenta pousar uma caixa à sua esquerda
    |(encontrapeca4 m (x+1,y-1) == Just Vazio || (encontrapeca4 m (x+1,y-1) == Nothing && encontrapeca4 m (x+1,y) /= Nothing)) && blocoabaixo' (x+1,y-1) m  = Jogo (alteramapa Caixa (blocoabaixo (x+1,y-1) m) m []) (Jogador (x,y) Este False) -- Se a peça à direita da caixa for Vazia, a caixa é largada acima do primeio bloco abaixo da caixa nessa coluna 
    |otherwise = (Jogo m (Jogador (x,y) Este True)) --Caso contrário, o jogo não se altera

-- |Esta função altera o mapa dado as coordenadas e a peça pela qual queremos substitituir
alteramapa :: Peca -> Coordenadas -> Mapa -> Mapa -> Mapa
alteramapa p (x,0) (l:ls) m = m ++ [(alteramapa' p x l [])] ++ ls -- Ao encontrarmos a linha onde se encontra o bloco que queremos mudar, construímos o resto do mapa e inserimos a linha que vai ser alterada
alteramapa p (x,y) (l:ls) m = alteramapa p (x,y-1) ls (m ++ [l]) -- Se ainda não encontramos a linha, retiramos a primeira linha e introduzimo-la num acumulador

-- |Função auxiliar da alteramapa que troca uma peça numa linha pela que desejamos
alteramapa' :: Peca -> Int -> [Peca] -> [Peca] -> [Peca]
alteramapa' p 0 (pe:ps) l = l ++ [p] ++ ps -- Quando encontramos o elemento da coluna que procuramos, trocamos esse elemento pelo que pretendemos colocar nesse lugar
alteramapa' p x (pe:ps) l = alteramapa' p (x-1) ps (l ++ [pe]) -- Se ainda não encontramos a coluna, retiramos o primeiro elemento e introduzimo-lo num acumulador


-- ===Auxiliares utilizadas em mais que um tipo de movimento
-- |Esta função encontra a peça em determinada cooredenada (se a mesma existir)
encontrapeca4 :: Mapa -> Coordenadas -> Maybe Peca
encontrapeca4 [] _ = Nothing -- Se as coordenadas não existirem no mapa, a função irá retornar Nothing
encontrapeca4 (l:ls) (x,0) = encontrapeca4' l x -- Se a função tiver eliminado o número de linhas (listas) para ter chegado à linha procurada, irá procurar dentro dessa linha (lista)
encontrapeca4 (l:ls) (x,y) = encontrapeca4 ls (x,y-1) -- Se a função não tiver chegado à linha procurada (O y da coordenada é usado como um "acumulador" de forma a saber quantas linhas é necessário eliminar (ex. se procurarmos uma peça na linha 2 teremos de eliminar a linha 0 e a 1, ou seja 2 linhas, logo podemos utilizar a coordenada como forma de saber qual é a linha da cabeça da lista indiretamente))

-- |Esta função auxiliar à encontrapeca4 encontra a peça sabendo qual é a linha da peça
encontrapeca4' :: [Peca] -> Int -> Maybe Peca
encontrapeca4' [] _ = Nothing -- Se as coordenadas não existirem no mapa, a função irá retornar Nothing
encontrapeca4' (l:ls) 0 = Just l -- Se a função tiver eliminado o número de elementos (peças) para ter chegado à coluna (x) procurada, irá retornar essa peça como a peça procurada
encontrapeca4' (l:ls) x = encontrapeca4' ls (x-1) -- Se a função ainda não tiver chegado à peça procurada, irá retirar um elemento à lista e procurar de novo (x é usado como um "acumulador" de forma a saber quantos elementos é necessário eliminar (ex. se procurarmos uma peça na coluna 2 teremos de eliminar a lcoluna 0 e a 1, ou seja 2 colunas, logo podemos utilizar a coordenada como forma de saber qual é a coluna da cabeça da lista indiretamente e saber se é essa a que procuramos))

-- |Esta função encontra o bloco abaixo mais próximo de dadas coordenadas
blocoabaixo :: Coordenadas -> Mapa -> Coordenadas
blocoabaixo (x,y) m
    |encontrapeca4 m (x,y+1) == Just Bloco || encontrapeca4 m (x,y+1) == Just Caixa = (x,y) -- Se for encontrado um bloco ou uma caixa no bloco abaixo das coordenadas avaliadas, estas últimas serão o resultado
    |otherwise = blocoabaixo (x,y+1) m -- Caso contrário, a função irá procurar por blocos abaixo das cooordenadas abaixo

-- |Esta função verifica se a primeira peça nao vazia abaixo é uma porta ou não
blocoabaixo' :: Coordenadas -> Mapa -> Bool
blocoabaixo' (x,y) m
    |encontrapeca4 m (x,y+1) == Just Bloco || encontrapeca4 m (x,y+1) == Just Caixa = True -- Se for encontrado um bloco ou uma caixa no bloco abaixo das coordenadas avaliadas, é possível largar a caixa
    |encontrapeca4 m (x,y+1) == Just Porta = False -- Caso contrário, não é possível largar a caixa
    |otherwise = blocoabaixo' (x,y+1) m -- Caso seja a peça seja vazia, a função irá procurar por blocos abaixo das cooordenadas abaixo


