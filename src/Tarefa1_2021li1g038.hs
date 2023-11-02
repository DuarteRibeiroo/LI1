{- |
Module      : Tarefa1_2021li1g038
Description : Validação de um potencial mapa
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g038 where

import LI12122
import Data.List

--  == Função Principal
-- |A função validaPotencialMapa é a função principal, sendo a única função da mesma verificar se todas as funções auxiliares que verificam as condições necessárias para a validação do mapa retornam os valores corretos
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas 
    |pecarep pecas && length (nportas pecas) == 1 && vercaixas pecas && verblocos pecas && evazio pecas = True
    |otherwise = False

-- == Funções auxiliares

-- === Primeira condição (Não haver mais do que uma declaração de peça para a mesma posição)
-- |Esta função verifica se não existe alguma coordenada onde estão declaradas duas peças
pecarep :: [(Peca, Coordenadas)] -> Bool
pecarep [] = True -- Se não existirem peças as mesmas não podem estar em sobreposição
pecarep ((_,(x,y)):xs) 
    |verpecas (x,y) xs == True = pecarep xs 
    |otherwise = False -- Se não existir outra declaração para esta coordenada, irá ser feito o mesmo processo entre o primeiro elemento do resto da lista e o restante da lista

-- | Esta função auxiliar determina se, dada uma coordenada específica, não existe nenhuma outra peça declarada nessa mesma coordenada
verpecas :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
verpecas (_,_) [] = True 
verpecas (x,y) ((_,(x2,y2)):xs)
    |x == x2 && y == y2 = False -- Se houverem duas peças iguais a condição é falsa
    |otherwise = verpecas (x,y) xs -- No caso contrário, o primeiro elemento é comparado com o primeiro elemento do restante da lista

-- === Segunda condição (Declarar exactamente uma porta)
-- |Esta função enumera as portas existentes no mapa (Retorna a lista de todas as portas em vez de apenas o número para poder ser reutilizada na 5a condição)
nportas :: [(Peca, Coordenadas)] -> [Coordenadas] 
nportas [] = [] -- Se o mapa não tiver peças, não tem portas
nportas ((p,(x,y)):xs)
    |p == Porta = (x,y): nportas xs -- Se nesta coordenada existir uma porta, o programa adiciona as suas coordenadas à lista, para além de verificar possíveis portas existentes no resto do mapa
    |otherwise = nportas xs

--- === Terceira condição (Todas as caixas devem estar posicionadas em cima de outra caixa ou bloco)
-- |Esta função verifica se todas as caixas estão em cima de um bloco
vercaixas :: [(Peca, Coordenadas)] -> Bool
vercaixas l = vercaixas' l (encontracaixas l) -- É invocada esta função auxiliar devido à necessidade de uma variável extra (A lista da localização de todas as caixas)

-- |Esta função auxiliar tem o mesmo propósito da vercaixas
vercaixas' :: [(Peca, Coordenadas)] -> [Coordenadas] -> Bool
vercaixas' _ [] = True -- Se não existem caixas, não existem caixas colocadas indevidamente
vercaixas' l ((x,y):xs)
    |encontrapeca (x,(y+1)) l == Bloco || encontrapeca (x,(y+1)) l == Caixa = vercaixas' l xs -- Se a peca da coordenada imediatamente abaixo de uma caixa for um bloco ou outra caixa, está colocada corretamente e a função irá verficar as outras caixas existentes no mapa 
    |otherwise = False -- Se a peça abaixo não for um bloco, o mapa não é válido

-- |Esta função encontra todas as coordenadas onde se situam caixas e regista as mesmas numa lista
encontracaixas :: [(Peca, Coordenadas)] -> [Coordenadas]
encontracaixas [] = [] -- Se não houver peças não vazias, também não há caixas
encontracaixas ((p,(x,y)):xs)
    |p == Caixa = (x,y):encontracaixas xs -- Se encontrar uma caixa, irá registar as coordenadas da mesma na lista
    |otherwise = encontracaixas xs -- Caso contrário, ignorará essa peça e procurará por caixas no resto da lista

-- |Esta função encontra o tipo de peça existente em dada coordenada (Assumindo que essa dada coordenada existe no mapa)
encontrapeca :: Coordenadas -> [(Peca, Coordenadas)] -> Peca
encontrapeca _ [] = Vazio -- Se a coordenada não está declarada é porque está ocupada por um espaço vazio
encontrapeca (x,y) ((p,(x2,y2)):xs)
    |x == x2 && y == y2 = p -- Se for encontrada uma coordenada com peça, o programa irá retornar essa peça como valor
    |otherwise = encontrapeca (x,y) xs

-- === Quarta condição (Devem existir espaços vazios)
-- |Esta função verifica se existe pelo menos um espaço vazio (declarado ou não) no mapa
evazio :: [(Peca, Coordenadas)] -> Bool
evazio x 
    |vernaodec (veraltura x) (verlargura x) x = True -- A função irá primeiro procurar se existem peças não declaradas, pois se existirem serão vazias
    |otherwise = vervazio x -- Caso não existam, a função irá procurar peças vazias que foram declaradas

-- |Esta função verifica se existem peças não declaradas (vazias) no mapa (partindo do princípio que não existem peças repetidas)
vernaodec :: Int -> Int -> [(Peca, Coordenadas)] -> Bool
vernaodec a l x
    |a*l == length x = False -- Se a altura * largura do mapa (número total de peças) for igual ao tamanho da lista de peças (número de peças declaradas), então não há peças não declaradas no mapa
    |otherwise = True --Caso contrário, o mapa tem peças não declaradas 
    
-- |Esta função verifica se existem elementos vazios declarados
vervazio :: [(Peca, Coordenadas)] -> Bool
vervazio [] = False -- Se não existem peças, não existem peças vazias declaradas
vervazio ((p,(_,_)):xs)
    |p == Vazio = True -- Se a peça for vazia, existem peças vazias
    |otherwise = vervazio xs

-- | Esta função identifica qual é o número de linhas (altura) de um mapa
veraltura :: [(Peca, Coordenadas)] -> Int
veraltura [] = 0
veraltura [(p,(x,y))] = y + 1 -- Se apenas existir uma peça declarada, o número de linhas (num mapa válido) tem de ser igual à linha dessa peça mais um (devido à primeira ser 0)
veraltura ((p,(x,y)):(p2,(x2,y2)):xs)
    |y >= y2 = veraltura ((p,(x,y)):xs) -- Se a primeira peça da lista pertencer a uma linha abaixo da segunda, esta será a maior linha dos elementos já avaliados
    |otherwise = veraltura ((p2,(x2,y2)):xs) -- Se a maior da duas primeiras peças for a segunda, a função irá comparar a linha da mesma com as do resto da lista

-- |Análoga a veraltura, esta função identifica o número de colunas (largura) de um mapa
verlargura :: [(Peca, Coordenadas)] -> Int
verlargura [] = 0
verlargura [(p,(x,y))] = (x+1) -- Se apenas existir uma peça declarada, o número de colunas (num mapa válido) tem de ser igual à coluna dessa peça mais um (devido à primeira ser 0)
verlargura ((p,(x,y)):(p2,(x2,y2)):xs)
    |x >= x2 = verlargura ((p,(x,y)):xs) -- Se a primeira peça da lista pertencer a uma coluna à frente da segunda, esta será a maior coluna dos elementos já avaliados
    |otherwise = verlargura ((p2,(x2,y2)):xs) -- Se a maior coluna da duas primeiras peças for a da segunda, a função irá comparar a coluna da mesma com as do resto da lista

-- === Quinta condição (A base do mapa deve ser composta por blocos)
-- |Esta função verfica se existe chão contínuo (Assumimos que o chão se encontra abaixo da porta, se se encontrar acima da porta consideramos como teto (inválido))
verblocos :: [(Peca, Coordenadas)] -> Bool
verblocos m = vercont (encontrablocos m ((verlargura m)-1)) m -- Irá invocar a função auxiliar, de modo a incluir os blocos existentes na última coluna

-- |Verifica se existe um caminho ate ao outro lado para cada bloco na ultima coluna
vercont :: [Coordenadas] -> [(Peca, Coordenadas)] -> Bool
vercont [] _ = False -- Se não houverem blocos na última coluna, não pode haver chão contínuo
vercont (x:xs) m
    |cont x m [] [] == False = vercont xs m -- Se o bloco testado não estiver ligado à primeira coluna por blocos, a função irá procurar continuidade começando noutro bloco da coluna
    |otherwise = True --Se existir chão contínuo abaixo da porta, o mapa é válido

-- |Verifica se existe bloco adjacente na linha à esquerda, nesse caso verifica para a mesma condição para o bloco adjacente até chegar à primeira linha
cont :: Coordenadas -- ^Coordenadas para as quais estamos a avaliar continuidade à esquerda
        -> [(Peca, Coordenadas)] -- ^Mapa original (Para conseguirmos encontrar qualquer peça do mapa)
        -> [Coordenadas] -- ^Coordenadas por onde o caminho foi temporariamente considerado válido (Se encontrarmos uma parte inválida, a função irá recuar por este caminho e retirar coordenadas já atravessadas até encontrar outro caminho possível)
        -> [Coordenadas] -- ^Lista de Coordenadas impossíveis (Quando não é encontrado bloco na coluna à esquerda adjacente a este bloco, este bloco passa a ser considerado inválido para criar um chão contínuo, e já não é considerado possível ao avaliar a continuidade do mesmo)
        -> Bool -- ^Resultado (Se a partir das coordenadas dadas inicialmente é possível criar um chão contínuo)
cont (0,y) m l imp = verporta ((0,y):l) (head (nportas m)) -- Se encontrar um chão contínuo (Conseguir encontrar caminho contínuo da última à primeira coluna), irá verificar se a porta se encontra acima do chão (head é utilizado pois se o mapa for válido apenas irá existir uma porta, logo nesse caso head nportas == nportas)
cont (x,y) m (l:xl) imp -- Quando estamos a avaliar blocos fora da última coluna
    |encontrapeca ((x-1),(y+1)) m == Bloco && elem (x-1,y+1) imp == False = cont (baseblocos (x-1,y+1) m) m ((x,y):l:xl) imp -- Análoga à situaçao abaixo, mas com bloco diagonalmente abaixo
    |encontrapeca ((x-1),y) m == Bloco && elem (x-1,y) imp == False = cont (baseblocos (x-1,y) m) m ((x,y):l:xl) imp-- Se existir bloco diretamente à esquerda e a mesma não foi invalidada anteriormente, irá procurar se essa adjacente possui adjacente que não foi ainda invalidada, para além de adicionar esse bloco à lista de blocos que constituem o chão do mapa
    |encontrapeca ((x-1),(y-1)) m == Bloco && elem (x-1,y-1) imp == False = cont (baseblocos (x-1,y-1) m) m ((x,y):l:xl) imp -- Análoga à situaçao acima, mas com bloco diagonalmente acima
    |encontrapeca (x,(y-1)) m == Bloco = cont (x,(y-1)) m (l:xl) ((x,y):imp) -- Se não encontrar peça adjacente irá tentar encontrar peça adjacente ao bloco acima, se esse existir, para além de invalidar o bloco abaixo como caminho possível
    |otherwise = cont l m xl ((x,y):imp) -- Caso não exista bloco adjacente nem bloco acima, a função irá retornar ao bloco pertencente ao caminho e procurar blocos adjacentes a esse, para além de invalidar o bloco que estava a avaliar anteriormente
cont (x,y) m [] imp -- Quando estamos a avaliar blocos na última coluna (Análoga ao pattern match anterior)
    |encontrapeca ((x-1),y) m == Bloco && elem (x-1,y) imp == False = cont (baseblocos (x-1,y) m) m [(x,y)] imp
    |encontrapeca ((x-1),(y-1)) m == Bloco && elem (x-1,y-1) imp == False = cont (baseblocos (x-1,y-1) m) m [(x,y)] imp
    |encontrapeca ((x-1),(y+1)) m == Bloco && elem (x-1,y+1) imp == False = cont (baseblocos (x-1,y+1) m) m [(x,y)] imp
    |otherwise = False -- Se não houver blocos adjacentes, a função irá retornar falso para esta cadeia de blocos na última coluna e irá procurar continuidade em possíveis blocos acima

-- |Procura o bloco mais abaixo na coluna que ainda está adjacente à coordenada do bloco dada inicialmente
baseblocos :: Coordenadas -> [(Peca, Coordenadas)] -> Coordenadas
baseblocos (x,y) m
    |encontrapeca (x,y+1) m == Bloco = baseblocos (x,y+1) m -- Se existir bloco abaixo das coordenadas em questão, irá procurar se existe bloco abaixo desse 
    |otherwise = (x,y) -- Caso contrário, irá retornar essas coordenadas como sendo o bloco mais abaixo numa coluna contínua de blocos

--Verifica se a porta está acima do chão encontrado (Se estiver abaixo o mapa é impossível de completar)
verporta :: [Coordenadas] -> Coordenadas -> Bool
verporta [] _ = False -- Se não existem blocos, não a porta não pode estar acima deles
verporta ((x,y):xs) (xp,yp)
    |x /= xp = verporta xs (xp,yp) -- Se a coluna do primeiro elemento não foi igual à coluna onde se localiza a porta, a função irá procurar pelo bloco na coluna da porta no resto da lista
    |otherwise = if y > yp then True else False -- Quando encontra o bloco pertencente à coluna correta, verifica se este se situa abaixo da porta

-- Encontra todos os blocos da última coluna
encontrablocos :: [(Peca, Coordenadas)] -> Int -> [Coordenadas]
encontrablocos [] _ = [] -- Se não houver peças na lista, não podem existir blocos
encontrablocos ((p,(x,y)):xs) l
    |x == l && p == Bloco = (x,y): encontrablocos xs l --Se for encontrada uma peça pertencente à ultima coluna e a mesma for um bloco, irś ser adicionada à lista
    |otherwise = encontrablocos xs l -- Caso contrário, não será adicionada à lista