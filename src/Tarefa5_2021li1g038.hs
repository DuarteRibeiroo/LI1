{- |
Module      : Tarefa5_2021li1g038
Description : Aplicação Gráfica completa
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
Foi utilizada a documentação oficial do [Gloss](https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss.html)
Foi utilizada ajuda de outro aluno da UC para o playback de música
A maior dificuldade sentida foi a gestão de tempo de modo a conseguir adicionar tudo o que foi pedido antes da data limite
-}
module Main where

import LI12122
import Tarefa1_2021li1g038
import Tarefa2_2021li1g038
import Tarefa4_2021li1g038
import Assets
import Data.Maybe
import Data.List
import Data.String
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy)

data Estado = Estado
    {modo :: Int -- ^Indica em que parte ou menu do jogo o utilizador está
    ,nivel :: Jogo -- ^O nível que o jogador está a jogar
    ,ecraprincipal :: Picture -- ^Imagem utilizada no ecrã principal
    ,pereste :: Picture -- ^Imagem do jogador virado para este
    ,peroeste :: Picture -- ^Imagem do jogador virado para oeste
    ,blocoimg :: Picture -- ^Imagem de um bloco
    ,caixaimg :: Picture -- ^Imagem de uma caixahh
    ,portaimg :: Picture -- ^Imagem de uma porta
    ,estrelas :: Picture -- ^Imagem de estrelas, utilizadas no fundo
    ,menujogar :: Picture -- ^Imagem do menu de esolha de modo de jogo
    ,escolhaniveis :: Picture -- ^Imagem do menu de escolher níveis
    ,fimrecorde :: Picture -- ^Imagem quando um nível é completado com o menor número possível de movimentos
    ,fimnormal :: Picture -- ^Imagem quando um nível é completado sem o menor número possível de movimentos
    ,criamapa :: Picture -- ^Instruções para o criador de mapas
    ,grid :: Picture -- ^Grelha utilizada no criador de mapas
    ,escolhermapa :: Picture -- ^Imagem utilizada na procura e ao guardar mapas
    ,listaerros :: Picture -- ^Imagem onde erros no mapa são apresentados qao tentar gravar um mapa
    ,numeros :: [Picture] -- ^Imagens de números
    ,errosimg :: [Picture] -- ^Imagens dos tipos de erros
    ,tempo :: Float -- ^Tempo passado desde o ínicio do programa
    ,melhormov :: Int -- ^Menor quantidade de movimentos possível para completar o mapa
    ,movatuais :: Int -- ^Quantidade de movimentos já realizados ao completar um nível (Tambem é utilizado como o bloco selecionado no criador de mapa e recorde do jogador no selecionador de mapas)
    ,recordesmov :: [Int] -- ^Lista de todos os recordes pessoais do utilizador
    ,mapasg :: [(String,Jogo)] -- ^Lista de mapas criados guardados
    ,caixatexto :: String -- ^Texto da caixa de texto (também é utilizado para saber qual o mapa oficial em que o jogador está)
    ,handle :: (Handle,Handle) -- ^Handle do ficheiro com os mapas guardados e do ficheiro com recordes pessoais
    ,erros :: [Int] -- ^Erros obtidos na criação de mapas (o mapa tem de ser possível)
    }

-- =Criação de estado
-- |Cria um estado utilizando as informações base
criaestado :: Int -> Jogo -> [Picture] -> [Picture] -> [Picture] -> Float -> Int -> Int -> [Int] -> [(String,Jogo)] -> (Handle,Handle) -> Estado
criaestado n j [ms,pere,pero,bl,ca,por,es,mjo,mni,fre,fno,criamapa,grid,escolhermapa,listaerros] numeros erros t mmov matuais recordespessoais mapasguardados h = Estado n j ms pere pero bl ca por es mjo mni fre fno criamapa grid escolhermapa listaerros numeros erros t mmov matuais recordespessoais mapasguardados [] h []

-- =Conversão gráfica de um estado
-- |Converte um estado numa representação gráfica (Picture)
convestado :: Estado -> IO Picture
convestado e 
    |modo e == 0 = return $ Pictures [criaespaco (tempo e) (estrelas e), (ecraprincipal e)] -- Modo 0 = Ecrã Principal
    |modo e == 1 = return $ Pictures [criaespaco (tempo e) (estrelas e), (menujogar e)] -- Modo 1 = Menu de escolher modo de jogo
    |modo e == 2 = return $ Pictures [criaespaco (tempo e) (estrelas e), (escolhaniveis e), Translate (-185) (-200) (Scale (4/(fromIntegral (length m))) (4/(fromIntegral (length m))) (Translate ((fromIntegral (-length (head m))/2)*50) (((fromIntegral (length m))/2)*50) gm)), Translate 460 (-300) (Scale 0.8 0.8 (graphnumeros (show (melhormov e)) (numeros e))), Translate 460 (-170) (Scale 0.8 0.8 (graphnumeros (show (movatuais e)) (numeros e)))] --  Modo 2 = Escolha de nível oficial (com preview do mapa escolhido)
    |modo e == 3 = return $ Pictures [criaespaco (tempo e) (estrelas e), graphjogo e] -- Modo 3 = Modo de jogo (Representa graficamente o jogo)
    |modo e == 4 = if melhormov e == movatuais e then return $ Pictures [criaespaco (tempo e) (estrelas e), fimrecorde e, (Translate  335 135 (Scale 0.80 0.80 (graphnumeros (show (movatuais e)) (numeros e))))] -- Modo 4 = Fim de jogo (Se o número de movimentos do jogador for o menor possível, irá mostrar esta imagem) 
                   else return $ Pictures [criaespaco (tempo e) (estrelas e), fimnormal e, (Translate  300 135 (Scale 0.85 0.85 (graphnumeros (show (movatuais e)) (numeros e)))), (Translate  40 (-40) (Scale 0.85 0.85 (graphnumeros (show (melhormov e)) (numeros e))))] -- Caso o número de movimentos necessários para completar o nível seja maior que o mínimo, irá mostrar esta imagem
    |modo e == 5 = return $ Pictures [criaespaco (tempo e) (estrelas e), Translate (-500) (-50) (Color white (Text (caixatexto e))), escolhermapa e] -- Modo 5 = Procura de nível guardado
    |modo e == 6 = return $ Pictures [criaespaco (tempo e) (estrelas e), criamapa e] -- Modo 6 = Instruções do criador de mapa
    |modo e == 7 = return $ Pictures [criaespaco (tempo e) (estrelas e), grid e , Translate (-640) 360 gm] -- Modo 7 = Criador de mapas (Contém uma grelha para orientação, centrar no canto superior esquerdo)
    |modo e == 8 = return $ Pictures [criaespaco (tempo e) (estrelas e), Translate (-500) (-50) (Color white (Text (caixatexto e))), escolhermapa e] -- Modo 8 = Guardar nível do criador de mapas
    |modo e == 9 = return $ Pictures [criaespaco (tempo e) (estrelas e), listaerros e, apresentaerros (erros e) (errosimg e)]
    where m = extraimapa (nivel e) -- Mapa atual
          j = extraijogador (nivel e) -- Jogador atual
          x = fromIntegral $ fst (extraicoordenadas (nivel e)) -- Coordenada X do jogador
          y = fromIntegral $ snd (extraicoordenadas (nivel e)) -- Coordenada Y do jogador
          gm = Pictures [graphmapa m (blocoimg e,caixaimg e,portaimg e) (length m), colocajogador j (pereste e,peroeste e,caixaimg e)] -- Representação gráfica do mapa (não centrada)

-- == Representação dos níveis
-- |Representa gráficamente o estado de jogo, centrado de forma a caber no ecrã
graphjogo :: Estado -> Picture
graphjogo e
    |length m < 16 && length (head m) < 27 = Translate ((fromIntegral (-length (head m))/2)*50) (((fromIntegral (length m))/2)*50) (Pictures [graphmapa m (blocoimg e,caixaimg e,portaimg e) (length m),colocajogador j (pereste e,peroeste e,caixaimg e)]) -- Se o mapa for menor que 27x16 , não é necessário recentrar em função da posição do jogador, pois o mapa cabe todo na janela
    |length m < 16 && length (head m) > 27 = Translate (determinascrollx (round x) (length (head m))) (((fromIntegral (length m))/2)*50) (Pictures [graphmapa m (blocoimg e,caixaimg e,portaimg e) (length m),colocajogador j (pereste e,peroeste e,caixaimg e)]) -- Caso a largura seja grande o suficiente para não caber no ecrã, mas a altura baixa o sufiente para não necessitar de scroll, é apenas realizado scroll no eixo x
    |length m > 16 && length (head m) < 27 = Translate ((fromIntegral (-length (head m))/2)*50) (determinascrolly (round y) (length m))  (Pictures [graphmapa m (blocoimg e,caixaimg e,portaimg e) (length m),colocajogador j (pereste e,peroeste e,caixaimg e)]) -- Caso a altura seja grande o suficiente para não caber no ecrã, mas a largura baixa o sufiente para não necessitar de scroll, é apenas realizado scroll no eixo y
    |otherwise = Translate (determinascrollx (round x) (length (head m))) (determinascrolly (round y) (length m)) (Pictures [graphmapa m (blocoimg e,caixaimg e,portaimg e) (length m),colocajogador j (pereste e,peroeste e,caixaimg e)]) -- Caso tanto a altura como a largura sejam grandes demais, é realizado scroll em ambos os eixos
    where m = extraimapa (nivel e) -- Mapa atual
          j = extraijogador (nivel e) -- Jogador atual
          x = fromIntegral $ fst (extraicoordenadas (nivel e)) -- Coordenada X do jogador
          y = fromIntegral $ snd (extraicoordenadas (nivel e)) -- Coordenada Y do jogador

-- |Determina o scroll da largura, de modo ao jogador se encontrar sempre dentro do ecrã
determinascrollx :: Int -> Int -> Float
determinascrollx x l
    |x < 13 = fromIntegral (-640) -- Se o jogador se encontrar perto do limite esquerdo, é movido o suficiente para o limite do mapa alinhar com o limite da janela
    |x > l-13 = fromIntegral (-(l*50)+640) -- Se o jogador se encontrar perto do limite direito, é movido o suficiente para o limite do mapa alinhar com o limite da janela
    |otherwise = fromIntegral (-(x*50)) -- Caso o jogador se encontre no meio do mapa, é movido o suficiente para o jogador se encontrar no centro do ecrã

-- |Determina o scroll da altura, de modo ao jogador se encontrar sempre dentro do ecrã
determinascrolly :: Int -> Int -> Float
determinascrolly y a
    |y > a-8 =fromIntegral ((a*50)-360) -- Se o jogador se encontrar perto do limite inferior, é movido o suficiente para o limite do mapa alinhar com o limite da janela
    |y < 8 =  fromIntegral (360) -- Se o jogador se encontrar perto do limite superior, é movido o suficiente para o limite do mapa alinhar com o limite da janela
    |otherwise = fromIntegral (y*50)-- Caso o jogador se encontre no meio do mapa, é movido o suficiente para o jogador se encontrar no centro do ecrã

-- |Esta função extrai um mapa de um jogo
extraimapa :: Jogo -> Mapa
extraimapa (Jogo m j) = m

-- |Esta função extrai o jogador de um jogo
extraijogador :: Jogo -> Jogador
extraijogador (Jogo m j)= j

-- |Esta função extrai as coordenadas do jogador de um jogo
extraicoordenadas :: Jogo -> Coordenadas
extraicoordenadas (Jogo m (Jogador (x,y) _ _)) = (x,y)

-- |Cria uma representação gráfica do mapa
graphmapa :: Mapa -> (Picture,Picture,Picture) -> Int-> Picture
graphmapa [] _ _  = Blank -- Quando o mapa acabar, não adiciona mais nada à imagem
graphmapa (l:ls) (bl,ca,por) a = Pictures [graphlinha l (0,a-length (l:ls)) (bl,ca,por) , graphmapa ls (bl,ca,por) a] -- O mapa depois é convertido linha a linha

-- |Cria uma representação gráfica de cada linha do mapa
graphlinha :: [Peca] -> (Int,Int) -> (Picture,Picture,Picture) -> Picture
graphlinha [] _ _ = Blank -- Quando a linha acabar, não adiciona mais nada à imagem
graphlinha (p:ps) (x,y) (bl,ca,por)
    |p == Vazio = graphlinha ps (x+1,y) (bl,ca,por) -- Se a peça atual for vazia, não irá colocar nada e irá avançar para a peça (e coordenadas) à frente
    |p == Bloco = Pictures [desenhapeca (fromIntegral x,fromIntegral y) bl, graphlinha ps (x+1,y) (bl,ca,por)] -- Se a peça atual for um bloco, irá colocar a mesma nas coordenadas em questão e irá repetir o processo para a peça (e coordenadas) à frente
    |p == Caixa = Pictures [desenhapeca (fromIntegral x,fromIntegral y) ca, graphlinha ps (x+1,y) (bl,ca,por)] -- Se a peça atual for uma caixa, irá colocar a mesma nas coordenadas em questão e irá repetir o processo para a peça (e coordenadas) à frente
    |p == Porta = Pictures [desenhapeca (fromIntegral x,fromIntegral y) por, graphlinha ps (x+1,y) (bl,ca,por)] -- Se a peça atual for uma porta, irá colocar a mesma nas coordenadas em questão e irá repetir o processo para a peça (e coordenadas) à frente

-- |Coloca o jogador (e se necessário caixa)
colocajogador :: Jogador -> (Picture,Picture,Picture) -> Picture
colocajogador (Jogador (x,y) Este False) (pere,_,_) = desenhapeca (fromIntegral x,fromIntegral y) pere -- Se não existir caixa, é apenas colocado o jogador na posição e direção relevante
colocajogador (Jogador (x,y) Oeste False) (_,pero,_) = desenhapeca (fromIntegral x,fromIntegral y) pero
colocajogador (Jogador (x,y) Este True) (pere,_,ca) = Pictures [desenhapeca (fromIntegral x,fromIntegral y) pere, desenhapeca (fromIntegral x,fromIntegral y-1) ca] -- Caso contrário, é também desenhada uma caixa acima do jogador
colocajogador (Jogador (x,y) Oeste True) (_,pero,ca) = Pictures [desenhapeca (fromIntegral x,fromIntegral y) pero, desenhapeca (fromIntegral x,fromIntegral y-1) ca]

-- |Desenha uma peça dadas as coordenadas (do jogo) da mesma
desenhapeca :: (Float,Float) -> Picture -> Picture 
desenhapeca (x,y) p = Translate ((x+0.5)*50) (-(y+0.5)*50) p -- Irá ser colocada a peça nas coordenadas certas, depois de convertidas em coordenadas gloss (Uma peça é considerada como sendo 50x50 pixeis)

-- == Criação do efeito de fundo
-- |Esta função cria o espaço de fundo e scroll correspondente
criaespaco :: Float -> Picture -> Picture
criaespaco t es = Pictures [Translate (crialoop (-600) t) 0 (clusterestrelas es), Translate (crialoop 0 t) 0 (clusterestrelas es),Translate (crialoop 500 t) 0 (clusterestrelas es),Translate (crialoop 800 t) 0 (clusterestrelas es)] -- São criados quatro "clusters" de estrelas com certa distância um do outro de modo a preencher o ecrã na sua totalidade. A sua posição é determinada pela função crialoop.

-- |Cluster de estrelas (várias estrelas colocadas próximas umas das outras de forma a criar um grupo denso de estrelas)
clusterestrelas :: Picture -> Picture
clusterestrelas es = Pictures [(Scale 1.25 1.25 es),Translate 0 (-300) (Scale 1.25 1.25 es),Translate (-50) 100 (Scale 1.25 1.25 es),Translate (-50) 300 (Scale 1.25 1.25 es),Translate (-150) (-100) (Scale 1.25 1.25 es), Translate (-50) 250 (Scale 1.25 1.25 es),Translate (-150) (-200) (Scale 1.25 1.25 es),Translate (-200) (-250) (Scale 1.25 1.25 es),Translate (-200) 200 (Scale 1.25 1.25 es),Translate (-300) (-280) (Scale 1.25 1.25 es),Translate (-300) 250 (Scale 1.25 1.25 es)]

-- |Função que determina a posição de cada cluster, criando um loop
crialoop :: Float -> Float -> Float
crialoop n t
    |n + (t*50) < 1100 = (n + (t*50)) -- Se a posição do cluster for menor que 1100, nada será alterado
    |otherwise = crialoop (n-2100) t -- Caso contrário, o cluster já não estará no ecrã, logo será "puxado" para o início, onde voltará a aparecer, fazendo assim um loop infinito de scroll

-- |Acumula o tempo passado
addtempo :: Float -> Estado -> IO Estado
addtempo s e = return $ e {tempo = ((tempo e)+s)} -- Adiciona o tempo atual ao tempo já passado

-- ==Representação de números
-- |Transforma um número numa imagem desse número
graphnumeros :: String -> [Picture] -> Picture
graphnumeros n l
    |length n == 1 = graphalgarismo (head n) l -- Se o número for menor que 10, irá apenas tornar esse número numa imagem
    |otherwise = Pictures [graphalgarismo (head n) l, Translate 30 0 (graphnumeros (tail n) l)] -- Caso contrário, irá tornar o primeiro algarismo numa imagem, e repetir a função para os algarismos restantes, para além de os colocar mais à direita

-- |Transforma um algarismo numa imagem desse algarismo
graphalgarismo :: Char -> [Picture] -> Picture
graphalgarismo n l = case n of
                  '0' -> head l -- Caso o número seja 0, irá utilizar a primeira imagem da lista de números (0)
                  '1' -> (!!) l 1 -- Análogo a acima
                  '2' -> (!!) l 2
                  '3' -> (!!) l 3
                  '4' -> (!!) l 4
                  '5' -> (!!) l 5
                  '6' -> (!!) l 6
                  '7' -> (!!) l 7 
                  '8' -> (!!) l 8
                  '9' -> last l

-- == Representação de erros
-- | Apresenta erros na criação de mapas
apresentaerros :: [Int] -> [Picture] -> Picture 
apresentaerros [] _ = Blank -- Se não existirem mais erros para apresentar a função termina
apresentaerros (x:xs) e@[e1,e2,e3,e4,e5]
    |x == 1 =  Pictures [Translate 0 200 e1, Translate 0 (-110) (apresentaerros xs e)] -- Se o primeira condição não se cumprir, ela é apresentada tal como os outros erros presentes
    |x == 2 =  Pictures [Translate 0 200 e2, Translate 0 (-110) (apresentaerros xs e)] -- Análogo a acima
    |x == 3 =  Pictures [Translate 0 200 e3, Translate 0 (-110) (apresentaerros xs e)]
    |x == 4 =  Pictures [Translate 0 200 e4, Translate 0 (-110) (apresentaerros xs e)]
    |x == 5 =  Pictures [Translate 0 200 e5, Translate 0 (-110) (apresentaerros xs e)]

-- = Gestão de eventos
-- |Altera o estado do jogo conforme a tecla ou botão pressionado
reagevento :: Event -> Estado -> IO Estado
reagevento (EventKey (MouseButton LeftButton) Down _ (x,y)) e = navegamenu (modo e) (x,y) e -- Se for pressionado o botão direito do rato, irá se verificar se esse clique corresponde a alguma mudança de menu
reagevento (EventMotion (x,y)) e = if modo e == 2 then mostrapreview (x,y) e else return e -- O movimento do rato é monitorizado no menu de esolha de nível, de modo a mostrar uma preview dependendo do nível onde o cursor está
reagevento (EventKey (SpecialKey KeyUp) Down _ _)    e = if modo e == 3 then if verfim (nivel (e { nivel = posstrepar (nivel e)})) then fimjogo (e {movatuais = (movatuais e) + 1 }) else if posstrepar (nivel e) == nivel e then return e else return (e { nivel = posstrepar (nivel e),movatuais = (movatuais e) + 1}) else return e -- Se estiver no modo de jogo, o jogo irá verificar se o jogador chega à porta, se sim, altera o ecrã para o modo de fim de jogo. Também verifica se o movimento realizado altera a posição ou não (se nao alterar não muda a quantidade de movimentos)
reagevento (EventKey (SpecialKey KeyLeft) Down _ _)  e = if modo e == 3 then if verfim (nivel (e { nivel = movesquerda (nivel e)})) then fimjogo (e {movatuais = (movatuais e) + 1 }) else if movesquerda (nivel e) == nivel e then return e else return (e { nivel = movesquerda (nivel e),movatuais = (movatuais e) + 1}) else return e -- Análogo a acima
reagevento (EventKey (SpecialKey KeyRight) Down _ _) e = if modo e == 3 then if verfim (nivel (e { nivel = movedireita (nivel e)})) then fimjogo (e {movatuais = (movatuais e) + 1 }) else if movedireita (nivel e) == nivel e then return e else return (e { nivel = movedireita (nivel e),movatuais = (movatuais e) + 1}) else return e
reagevento (EventKey (SpecialKey KeySpace) Down _ _) e = if modo e == 3 then if pegacaixa (nivel e) == nivel e then return e else return (e { nivel = pegacaixa (nivel e),movatuais = (movatuais e) + 1}) else return e
reagevento (EventKey (SpecialKey KeyEnter) Down _ _) e 
    |modo e == 4 = return $ e {modo = 0,nivel = jvazio,caixatexto = []}  -- Se Enter for pressionado no ecrã de fim de jogo, o jogo volta ao ecrã inicial e reinicia o mapa
    |modo e == 5 = if procuramapa (caixatexto e) (mapasg e) == Jogo [] (Jogador (0,0) Oeste False) then return $ e {modo = 0,caixatexto = []} else return $ e {modo = 3,nivel = procuramapa (caixatexto e) (mapasg e),caixatexto = []} -- Se enter for pressionado no ecrã de procura de nível guardado. Se um nível for encontrado, inicia o jogo nesse nível. Caso contrário, leva o jogador para o menu inicial
    |modo e == 8 = guardamapa e -- Guarda o mapa após ser introduzido o nome
    |modo e == 9 = return $ e {modo = 7} -- Quando o mapa é impossível leva de volta ao editor
    |otherwise = return e -- Caso seja pressionado noutro modo, o estado nao se altera
reagevento (EventKey (SpecialKey KeyHome) Down _ _) e = if modo e == 5 || modo e == 8 then if caixatexto e == [] then return $ e {caixatexto = [] } else return $ e {caixatexto = init (caixatexto e)} else return e -- No modo de procura de mapas ou nomeação de mapas, apaga um caracter da caixa de procura
reagevento (EventKey (Char k) Down _ _) e 
    |modo e == 3 = if k == 'Q' then return $ e {modo = 0,nivel = jvazio,melhormov = 0, movatuais = 0,caixatexto = []} else return e -- Se o jogador premir Q durante o jogo, desiste e é levado para o ecrã principal
    |modo e == 5 || modo e == 8 = return $ e {caixatexto = (caixatexto e) ++ [k]} -- No modo de procura de mapas ou de nomear mapas, adiciona esse caracter a caixa de texto
    |modo e == 7 = case k of
                    '1' -> return $ e {movatuais = 1} -- No modo de criar mapas, altera a peça escolhida para Bloco
                    '2' -> return $ e {movatuais = 2} -- No modo de criar mapas, altera a peça escolhida para Caixa
                    '3' -> return $ e {movatuais = 3} -- No modo de criar mapas, altera a peça escolhida para Porta
                    '4' -> return $ e {movatuais = 4} -- No modo de criar mapas, altera a peça escolhida para Vazio
                    '5' -> return $ e {movatuais = 5} -- No modo de criar mapas, altera para a escolha da posição do jogador virado para Este
                    '6' -> return $ e {movatuais = 6} -- No modo de criar mapas, altera para a escolha da posição do jogador virado para Oeste
                    'S' -> if verificanivel (desconstroiMapa (extraimapa (nivel e))) (extraicoordenadas (nivel e)) [] == [] then return $ e {modo = 8} else return $ e {modo = 9,erros = verificanivel (desconstroiMapa (extraimapa (nivel e))) (extraicoordenadas (nivel e)) []} -- No modo de criar mapas,envia o jogador para nomear o mapa
                    'L' -> return $ e {modo = 0, nivel = jogo1} -- Volta ao menu principal
                    _ -> return e -- Ignora qualquer outra tecla
    |modo e == 9 = if k == 'c' then return $ e {modo = 8} else return e--Override à deteção de mapas impossíveis
    |otherwise = return e -- Ignora qualquer outro modo
reagevento _ e = return e -- Ignora qualquer outra tecla

-- == Gestão de cliques
-- |Esta função determina a ação a realizar dependendo do modo atual e do local onde o cursor do rato estava quando foi clicado (localização dos botões está armazenado no módulo Assets)
navegamenu :: Int -> (Float,Float) -> Estado -> IO Estado
navegamenu 0 (x,y) e = case encontrabotao (x,y) botoesmenu 1 of -- Casos no menu principal
                            0 -> return e -- Se o local clicado não corresponder a nenhum botão, nada se irá alterar
                            1 -> return $ e {modo = 1} -- Se o primeiro botão for clicado, o menu será alterado para o de esolha de tipo de jogo
                            2 -> return $ e {modo = 6} -- Se o segundo botão for clicado, serão mostradas as instruções do criador de mapa
                            3 -> do spawnCommand "killall mpv"
                                    exitSuccess -- Se o terceiro botão for clicado, o jogo é terminado
navegamenu 1 (x,y) e = case (encontrabotao (x,y) botoesmenujogo 1) of -- Casos na escolha de tipo de jogo
                            0 -> return e -- Se o local clicado não corresponder a nenhum botão, nada se irá alterar
                            1 -> return $ e {modo = 2} -- Se o primeiro botão for pressionado, irá ser iniciado o menu de escolha de nível oficial
                            2 -> return $ e {modo = 5} -- Se o segundo botão for clicado, serão mostradas as instruções do criador de mapas
                            3 -> return e
navegamenu 2 (x,y) e = case (encontrabotao (x,y) botoesniveis 1) of -- Casos no menu de escolha de nível
                            0 -> return e -- Se o local clicado não corresponder a nenhum botão, nada se irá alterar
                            1 -> return $ e {modo = 3,movatuais = 0,caixatexto = "1"} -- Se o primeiro botão for pressionado, será iniciado o primeiro nível (o número de movimentos atuais é reiniciado)
                            2 -> return $ e {modo = 3,movatuais = 0,caixatexto = "2"} -- Análogo a acima
                            3 -> return $ e {modo = 3,movatuais = 0,caixatexto = "3"}
                            4 -> return $ e {modo = 3,movatuais = 0,caixatexto = "4"}
                            5 -> return $ e {modo = 3,movatuais = 0,caixatexto = "5"}
                            6 -> return $ e {modo = 3,movatuais = 0,caixatexto = "6"}
                            7 -> return $ e {modo = 3,movatuais = 0,caixatexto = "7"}
                            8 -> return $ e {modo = 3,movatuais = 0,caixatexto = "8"}
                            9 -> return $ e {modo = 3,movatuais = 0,caixatexto = "9"}
                            10 -> return $ e {modo = 3,movatuais = 0,caixatexto = "10"}
                            11 -> return $ e {modo = 3,movatuais = 0,caixatexto = "11"}
                            12 -> return $ e {modo = 3,movatuais = 0,caixatexto = "12"}
                            13 -> return $ e {modo = 3,movatuais = 0,caixatexto = "13"}
                            14 -> return $ e {modo = 3,movatuais = 0,caixatexto = "14"}
navegamenu 6 (x,y) e = if (encontrabotao (x,y) botoesinstrucoes 1) == 1 then return $ e {modo = 7,nivel = base, caixatexto = [],movatuais = 1} else return e -- Se o botão nas instruções for clicado, o criador de mapas é carregado com um mapa básico e fica pronto a colocar blocos
navegamenu 7 (x,y) e = case movatuais e of 
                            1 -> return $ e {nivel = substituimapa (nivel e) (alteramapa Bloco (xo,yo) (extraimapa (nivel e)) [])}-- Se o botão do rato for pressionado enquanto o tipo de peça for Bloco (movatuais for igual a 1), irá converter as coordenadas Gloss em coordenadas de Jogo e a peça nesse local irá ser substituída por um bloco
                            2 -> return $ e {nivel = substituimapa (nivel e) (alteramapa Caixa (xo,yo) (extraimapa (nivel e)) [])}-- Análogo a acima com peça diferente
                            3 -> return $ e {nivel = substituimapa (nivel e) (alteramapa Porta (xo,yo) (extraimapa (nivel e)) [])}
                            4 -> return $ e {nivel = substituimapa (nivel e) (alteramapa Vazio (xo,yo) (extraimapa (nivel e)) [])}
                            5 -> return $ e {nivel = substituijogador (nivel e) (xo,yo) False}-- Análogo a acima, mas substitui a posição do jogador
                            6 -> return $ e {nivel = substituijogador (nivel e) (xo,yo) True}
                            _ -> return e
                            where xo = if fromInteger (round ((x+640)/50)) > ((x+640)/50) then (round ((x+640)/50)) - 1 else round ((x+640)/50)-- Converte coordenadas x de gloss em coordenadas x de Jogo ("Puxa" as coordenadas para a frente "meio ecrã" (1280/2), visto que o x positivo em gloss começa a meio do ecrã e as coordenadas de jogo são apenas positivas, e de seguida divide as mesmas por 50 (cada peça tem o tamanho de 50x50 pixeis enquanto as coordenadas de jogo são 1x1). Ao converter as coordenadas de Float para Int, é realizado o arredondamento, logo é criada uma condição para o arredondamento ser sempre realizado por defeito e não excesso)
                                  yo = if fromInteger (round ((-y+360)/50)) > ((-y+360)/50) then (round ((-y+360)/50)) - 1 else round ((-y+360)/50)-- Converte coordenadas y de gloss em coordenadas y de Jogo ("Puxa" as coordenadas para a baixo "meio ecrã" (720/2), visto que o y negativo em gloss começa a meio do ecrã, e é também invertido o seu sinal, visto que o eixo y no Jogo aponta para baixo e o das coordenadas Gloss para cima, e de seguida divide as mesmas por 50 (cada peça tem o tamanho de 50x50 pixeis enquanto as coordenadas de jogo são 1x1). Ao converter as coordenadas de Float para Int, é realizado o arredondamento, logo é criada uma condição para o arredondamento ser sempre realizado por defeito e não excesso)
                                  substituimapa (Jogo m j) m2 = Jogo m2 j -- Substitui um mapa de um determinado jogo por outro mapa
                                  substituijogador (Jogo m (Jogador (x,y) _ c)) (x2,y2) b = if b then Jogo m (Jogador (x2,y2) Oeste c) else Jogo m (Jogador (x2,y2) Este c) -- Substitui a posição e orientação de um jogador de um determinado Jogo
navegamenu _ _ e = return e -- Ignora qualquer outro clique

-- |Esta função encontra o botão clicado a partir das coordenadas do rato e da lista da localização dos botões
encontrabotao :: (Float,Float) ->  [((Float,Float),(Float,Float))] -> Int -> Int
encontrabotao _ [] _ = 0 -- Se a posição do rato não corresponder a nenhum botão, a função irá retornar 0
encontrabotao (x,y) (((xp1,yp1),(xp2,yp2)):br) nb
    |(xp1 < x && x < xp2) && (yp1 > y && y > yp2) = nb -- Se a localização do rato estiver dentro dos limites do botão, irá ser esse o botão selecionado
    |otherwise = encontrabotao (x,y) br (nb+1) -- Caso contrário irá procurar nos outros botões (sendo nb o acumulador de modo a saber qual o número do botão)

-- == Gestão de movimentos do cursor do rato
-- |Altera o mapa mostrado dependendo das coordenadas do cursor
mostrapreview :: (Float,Float) -> Estado -> IO Estado
mostrapreview (x,y) e = case (encontrabotao (x,y) botoesniveis 1) of
                            0 -> return e -- Se o cursor não estiver por cima de nenhum mapa, o mapa mostrado na preview não se altera
                            1 -> return $ e {nivel= jogo1,melhormov = head melhormovpossivel,movatuais = head (recordesmov e)} -- Se o cursor não estiver por cima do primeiro nível , o mapa mostrado na preview é alterado para o primeiro
                            2 -> return $ e {nivel= jogo2,melhormov = (!!) melhormovpossivel  1,movatuais = (!!) (recordesmov e) 1} -- Análogo a acima
                            3 -> return $ e {nivel= jogo3,melhormov = (!!) melhormovpossivel  2,movatuais = (!!) (recordesmov e) 2}
                            4 -> return $ e {nivel= jogo4,melhormov = (!!) melhormovpossivel  3,movatuais = (!!) (recordesmov e) 3}
                            5 -> return $ e {nivel= jogo5,melhormov = (!!) melhormovpossivel  4,movatuais = (!!) (recordesmov e) 4}
                            6 -> return $ e {nivel= jogo6,melhormov = (!!) melhormovpossivel  5,movatuais = (!!) (recordesmov e) 5}
                            7 -> return $ e {nivel= jogo7,melhormov = (!!) melhormovpossivel  6,movatuais = (!!) (recordesmov e) 6}
                            8 -> return $ e {nivel= jogo8,melhormov = (!!) melhormovpossivel  7,movatuais = (!!) (recordesmov e) 7}
                            9 -> return $ e {nivel= jogo9,melhormov = (!!) melhormovpossivel  8,movatuais = (!!) (recordesmov e) 8}
                            10 -> return $ e {nivel= jogo10,melhormov = (!!) melhormovpossivel  9,movatuais = (!!) (recordesmov e) 9}
                            11 -> return $ e {nivel= jogo11,melhormov = (!!) melhormovpossivel  10,movatuais = (!!) (recordesmov e) 10}
                            12 -> return $ e {nivel= jogo12,melhormov = (!!) melhormovpossivel  11,movatuais = (!!) (recordesmov e) 11}
                            13 -> return $ e {nivel= jogo13,melhormov = (!!) melhormovpossivel  12,movatuais = (!!) (recordesmov e) 12}
                            14 -> return $ e {nivel= jogo14,melhormov = last melhormovpossivel , movatuais = last (recordesmov e)}

-- =Completação de níveis e criador de mapas
-- |Verifica se o jogador completou o nível
verfim :: Jogo -> Bool
verfim (Jogo m (Jogador (x,y) d ca))
    |encontrapeca4 m (x,y) == Just Porta = True --Se o jogador estiver numa porta, significa que completou o nível
    |otherwise = False -- Caso contrário, ainda não completou o nível

-- |Decide o que é necessário fazer quando o jogo acaba
fimjogo :: Estado -> IO Estado
fimjogo e = case caixatexto e of
            "1" -> if movatuais e < head (recordesmov e) || head (recordesmov e) == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4}) -- Se o número de movimentos for um recorde, guarda esse número. Caso contrário, não guarda o número e apenas finaliza o jogo
            "2" -> if movatuais e < (!!) (recordesmov e) 1 || (!!) (recordesmov e) 1 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4}) -- Análogo a acima
            "3" -> if movatuais e < (!!) (recordesmov e) 2 || (!!) (recordesmov e) 2 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "4" -> if movatuais e < (!!) (recordesmov e) 3 || (!!) (recordesmov e) 3 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "5" -> if movatuais e < (!!) (recordesmov e) 4 || (!!) (recordesmov e) 4 == 0then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "6" -> if movatuais e < (!!) (recordesmov e) 5 || (!!) (recordesmov e) 5 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "7" -> if movatuais e < (!!) (recordesmov e) 6 || (!!) (recordesmov e) 6 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "8" -> if movatuais e < (!!) (recordesmov e) 7 || (!!) (recordesmov e) 7 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "9" -> if movatuais e < (!!) (recordesmov e) 8 || (!!) (recordesmov e) 8 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "10" -> if movatuais e < (!!) (recordesmov e) 9 || (!!) (recordesmov e) 9 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "11" -> if movatuais e < (!!) (recordesmov e) 10 || (!!) (recordesmov e) 10 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "12" -> if movatuais e < (!!) (recordesmov e) 11 || (!!) (recordesmov e) 11 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "13" -> if movatuais e < (!!) (recordesmov e) 12 || (!!) (recordesmov e) 12 == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            "14" -> if movatuais e < last (recordesmov e) || last (recordesmov e) == 0 then guardarecordes (e {modo = 4}) else return (e {modo = 4})
            _ -> return (e {modo = 0}) -- Se o nível não for oficial os movimentos não são registados

-- |Verifica se o nível construído não é impossível (usando os critérios da Tarefa 1, exceto o das peças declaradas duas vezes), adicionando todos os critérios que falha a uma lista
verificanivel :: [(Peca,Coordenadas)] -> (Coordenadas) -> [Int] -> [Int]
verificanivel m (x,y) l 
    |verblocos m == False && elem 5 l == False = verificanivel m (x,y) (5:l) -- Se o mapa não tiver chão contínuo, falha esta verificação (se já tiver feito esta verificação, ou qualquer outra consequente, anteriormente ignora-a)
    |evazio m == False && elem 4 l == False = verificanivel m (x,y) (4:l) -- Se o mapa não tiver espaços vazios, falha esta verificação
    |vercaixas m == False && elem 3 l == False = verificanivel m (x,y) (3:l) -- Se existirem caixas flutuantes, falha esta verificação
    |length (nportas m) /= 1 && elem 2 l == False = verificanivel m (x,y) (2:l) -- Se existir mais de uma porta, falha esta verificação
    |(encontrapeca (x,y) m /= Vazio || x >= verlargura m || y >= veraltura m) && elem 1 l == False = verificanivel m (x,y) (1:l) -- Se o jogador estiver fora do mapa ou dentro de um bloco, falha esta verificação
    |otherwise = l

-- = Leitura e carregamento de mapas guardados

-- |Adiciona um mapa ao ficheiro de mapas criados
guardamapa :: Estado -> IO Estado
guardamapa e = do
                hClose (fst (handle e)) -- É fechado o ficheiro de modo a poder ser iniciado para adicionar o mapa
                appendFile "niveiscriados.txt" (comprimejogo (caixatexto e) (Jogo (cropvazio (extraimapa (nivel e))) (extraijogador (nivel e)))) -- O mapa é comprimido e de seguida adicionado ao ficheiro
                m <- openFile "niveiscriados.txt" ReadMode -- É reaberto o ficheiro
                mapas <- hGetContents m -- Os mapas são recarregados
                let mapasguardados = listamapas [] (lines mapas) -- Os mapas são descomprimidos e readicionados ao jogo 
                return $ e {modo = 0, nivel = jvazio,mapasg = mapasguardados,caixatexto = [],handle = (m,snd (handle e)),erros = []}-- O estado é retornado para continuar o funcionamento do jogo

-- |Retira as linhas e colunas vazias desnecessárias do mapa (Assumindo que o mapa está "encostado" no canto superior esquerdo)
cropvazio :: Mapa -> Mapa
cropvazio m = if checkvazio (last m) then cropvazio (init m) else colunasvazias m -- São procuradas e eliminadas todas as linhas vazias, e de seguida são eliminadas as colunas completamente vazias

-- |Elimina as colunas completamente vazias da direita para a esquerda até encontrar alguma peça
colunasvazias :: Mapa -> Mapa
colunasvazias m = map (take (length (head m)-(minimum (map (contavazios 0) m)))) m -- Inicialmente são contados os espaços vazios à direita até ser encontrada outra peça em cada linha, e são eliminadas de todas o número de colunas correspondente ao menor desses números (de modo a cortar todas as coluas só vazias, pois vão ser cortadas todas as colunas até ser encontrado um bloco)

-- |Conta a quantidade de vazios existentes à direita de cada coluna até ser encontrada uma peça
contavazios :: Int -> [Peca] -> Int
contavazios n [] = n -- Se chegar ao fim da linha, o número de peças vazias seguidas serão aquelas no acumulador
contavazios n (l:ls)
    |l == Vazio = contavazios (n+1) ls -- Se a peça for vazia, é adicionada ao acumulador
    |otherwise = contavazios 0 ls -- Caso contrário, a contagem é reiniciada, visto que queremos as peças vazias à direita de peças "sólidas", e como esta é sólida, nenhuma à sua esquerda poderá ser eliminada

-- |Verifica se a última linha é completamente vazia (pode ser eliminada)
checkvazio :: [Peca] -> Bool
checkvazio [] = True -- Se a linha for percorrida sem uma peça "sólida" ser encontrada a condição é cumprida
checkvazio (l:ls)
    |l == Vazio = checkvazio ls -- Se a peça for vazia, o resto da linha é verificada por peças vazias
    |otherwise = False -- Se a peça não for vazia, a linha não é completamente vazia

{- |Comprime um Jogo, junto com o seu nome, numa string
Os dados de mapa estão armazenados da seguinte forma
NOME-P1P2P3P4.P5P6P7P8(x,y)DC
Onde:
* NOME = Nome do mapa
* - = Caracter identificador da separação entre nome e mapa
* Px = Peça, que pode ser uma das quatro iniciais:
      1. B -> Corresponde a um bl

 X            
      5. '.' -> Corresponde ao fim de uma linha
* ( = Caracter identificador da separação entre mapa e coordenada x
* x = Coordenada x
* , = Caracter identificador da separação entre coordenada x e y
* y = Coordenada y
* ) = Caracter identificador da separação entre coordenadas e direção/estado de caixa
* D = Direção, que pode tanto ser Este (E) ou Oeste (O)
* C = Estado de caixa segurada, que tanto pode ser False (F)/não está a segurar numa caixa, e o seu inverso, True (T)


@
comprimejogo s (Jogo m (Jogador (x,y) dir ca)) = "\n" ++ s ++ "-" ++ (comprimemapa m) ++ "(" ++ (show x) ++ "," ++ (show y) ++ ")" ++ comprimedircai dir ca
@
=== Exemplo:
>>> comprimejogo "Mapa" (Jogo [[Vazio,Porta],[Bloco,Bloco]] (Jogador (0,0) Este False))
"Mapa-VP.BB(0,0)EF"
 -}
comprimejogo :: String -> Jogo -> String
comprimejogo s (Jogo m (Jogador (x,y) dir ca)) = "\n" ++ s ++ "-" ++ (comprimemapa m) ++ "(" ++ (show x) ++ "," ++ (show y) ++ ")" ++ comprimedircai dir ca

-- |Comprime um mapa e torna-o numa string
comprimemapa :: Mapa -> String
comprimemapa [[l]] -- Casos para quando é a última peca (evita colocar um ponto no final)
    |l == Vazio = "V" -- Análogo a abaixo
    |l == Bloco = "B"
    |l == Caixa = "C"
    |l == Porta = "P"
comprimemapa ((l:ls):xs)
    |l == Vazio = if ls == [] then "V." ++ comprimemapa xs else "V" ++ comprimemapa (ls:xs) -- Quando é a última peça da linha, é adicionado um ponto para significar essa mudança de linha, para além de um V para simbolizar Vazio
    |l == Bloco = if ls == [] then "B." ++ comprimemapa xs else "B" ++ comprimemapa (ls:xs) -- Análogo ao acima, mas com B para simbolizar um Bloco
    |l == Caixa = if ls == [] then "C." ++ comprimemapa xs else "C" ++ comprimemapa (ls:xs) -- Análogo ao acima, mas com C para simbolizar uma Caixa
    |l == Porta = if ls == [] then "P." ++ comprimemapa xs else "P" ++ comprimemapa (ls:xs) -- Análogo ao acima, mas com V para simbolizar uma Porta

-- |Comprime a direção e o status de segurar numa caixa de um jogador
comprimedircai :: Direcao -> Bool -> String
comprimedircai d ca
    |d == Oeste = if ca then "OT" else "OF" -- Se o jogador estiver virado para Oeste, verifica se está a segurar numa caixa ou não, colocando 'O' seguido da inicial do Bool da caixa ('T' ou 'F')
    |d == Este = if ca then "ET" else "EF" -- Análogo a acima, mas usa E para representar Este

-- |Descomprime os mapas, acumulando o nome até chegar ao mapa em si, e de seguida converte o mesmo de string para mapa, seguido da conversão dos detalhes do jogador
listamapas :: String -> [String] -> [(String,Jogo)]
listamapas _ [] = []
listamapas n ((x:xs):ls)
    |x == '-' = (n,Jogo (convertemapa [] xs) (Jogador (convertecoordenadax [] (eliminaate '(' xs),convertecoordenaday [] (eliminaate ',' xs)) (fst (convertertedircaixa xs)) (snd (convertertedircaixa xs)))) : (listamapas [] ls) -- Quando '-' é encontrado, sabe-se que o nome do mapa acabaou e após o mesmo está o mapa e o resto das informações do jogador
    |otherwise = listamapas (n ++ [x]) (xs:ls) -- Enquanto o nome do mapa é recolhido, é armazenado no acumulador

-- |Esta funçao elimina toda a string até certo caracter ser encontrado, quando retorna o resto da string
eliminaate :: Char -> String -> String
eliminaate c (x:xs)
    |c == x = xs -- Se o caracter em questão for encontrado , retorna o resto da string
    |otherwise =  eliminaate c xs -- Caso contrário, continua a eliminar caracteres

-- |Converte um mapa de uma string para um mapa
convertemapa :: [Peca] -> String -> [[Peca]]
convertemapa m (l:ls)
    |l == 'V' = convertemapa (m ++ [Vazio]) ls -- Caso o caracter seja V, irá colocar um Vazio
    |l == 'B' = convertemapa (m ++ [Bloco]) ls -- Caso o caracter seja B, irá colocar um Bloco
    |l == 'P' = convertemapa (m ++ [Porta]) ls -- Caso o caracter seja P, irá colocar uma Porta
    |l == 'C' = convertemapa (m ++ [Caixa]) ls -- Caso o caracter seja C, irá colocar uma Caixa
    |l == '.' = [m] ++ (convertemapa [] ls) -- -- Caso o caracter seja ., irá trocar de linha
    |otherwise = [m] -- Caso o mapa acabe (caracteres diferentes aos dos mapas), a função acaba

-- |Converte a string da coordenada x num número
convertecoordenadax :: String -> String-> Int
convertecoordenadax n (x:xs)
    |x == ',' = read n :: Int --  Ao encontrar o caracter ',', sabe que acabou os números relevantes as coordenadas x, logo converte o que acumulou
    |otherwise = convertecoordenadax (n ++ [x]) xs --Antes de encontrar esse caracter, vai adicionando os algarismos de x ao acumulador

-- |Converte a string da coordenada y num número
convertecoordenaday :: String -> String-> Int
convertecoordenaday n (x:xs)
    |x == ')' = read n :: Int --  Ao encontrar o caracter ')', sabe que acabou os números relevantes as coordenadas y, logo converte o que acumulou
    |otherwise = convertecoordenaday (n ++ [x]) xs --Antes de encontrar esse caracter, vai adicionando os algarismos de y ao acumulador

-- |Converte a string da direção e se estiver a segurar em caixa em dados utilizados no Jogo
convertertedircaixa :: String -> (Direcao,Bool)
convertertedircaixa (x:y:z:xs)
    |x == ')' = case y of -- ao encontrar o caracter ')', sabe que de serguida estão as informações da direçao e caixa
                'E' -> if z == 'F' then (Este,False) else (Este,True) -- Se a primeira letra for E, a direção é Este, e se a segunda for F, o jogador não está a segurar numa caixa, se a segunda for T, o jogador está a segurar numa caixa
                'O' -> if z == 'F' then (Oeste,False) else (Oeste,True) -- Se a primeira letra for O, a direção é Oeste, e se a segunda for F, o jogador não está a segurar numa caixa, se a segunda for T, o jogador está a segurar numa caixa
    |otherwise = convertertedircaixa (y:z:xs)-- Continua a procurar no resto da lista, se ainda não tiver alcançado o caracter ')'

-- |Procura um mapa dado o seu nome
procuramapa :: String -> [(String,Jogo)] -> Jogo
procuramapa _ [] = Jogo [] (Jogador (0,0) Oeste False) -- Se não encontrar mapa, retorna um mapa inválido, de modo a ser reconhecido pela função reagevento como inxistente
procuramapa n ((s,j):xs)
    |n == s = j -- Se encontrar mapa com esse nome, retorna esse nível 
    |otherwise = procuramapa n xs -- Caso contrário, procura no resto do mapa

-- =Guardar / Carregar recordes

{- |Guarda um recorde num ficheiro
Os recordes estão armazenados da seguinte forma: 
N-nmov
Onde : 
* N = Número correspondente ao mapa oficial
* '-' = Caracter separador do número de mapa e número recorde de movimentos
* nmov = Número recorde de movimentos para esse mapa
Todos os recordes estão permanentemente registados no ficheiro e vão sendo adicionados ao ficheiro 
-}
guardarecordes :: Estado -> IO Estado
guardarecordes e = do
                hClose (snd (handle e)) -- É fechado o ficheiro de modo a poder ser iniciado para adicionar os recordes atualizados
                appendFile "recordes.txt" (comprimerecorde (caixatexto e) (movatuais e)) -- O recorde é adicionado ao ficheiro
                re <- openFile "recordes.txt" ReadMode -- É reaberto o ficheiro
                recordes <- hGetContents re 
                let recordespessoais = carregarecordes (encontrarecordes (reverse (lines recordes)) 14) -- Os recordes são reconvertidos e atualizados
                return $ e {handle = (fst (handle e),re),recordesmov = recordespessoais} -- O estado é retornado para continuar o funcionamento do jogo

-- |Transforma os recordes de strings para números
carregarecordes :: [String] -> [Int]
carregarecordes r = reverse (map read (map (tail.dropWhile (/= '-') ) r)) -- O número do nível é removido dos recordes e de seguida são convertidos em números

-- | Encontra o recorde mais recente de cada um dos níveis, visto que existem vários recordes desatualizados
encontrarecordes :: [String] -> Int -> [String]
encontrarecordes l 0 = [] -- Após todos os recordes serem lidos a função termina
encontrarecordes l n = (fromJust (find (comecapor n) l)):(encontrarecordes l (n-1)) -- Vai encontrar os recordes mais recentes

-- |Verifica o nível de qual esse recorde se refere ( o número inicial)
comecapor :: Int -> String -> Bool
comecapor s l
    |s < 10 = if [head l] == show s then True else False -- Se o número só tiver um algarismo, apenas o primeiro caracter é avaliado
    |otherwise = if take 2 l == show s then True else False -- Caso contrário, avaliam-se os dois primeiros

-- |Transforma o recorde e o nível relevante numa string
comprimerecorde :: String -> Int -> String
comprimerecorde n r = "\n" ++ n ++ "-" ++ show r -- Começa numa linha nova, adiciona o número do nível, o caracter divisor e o número de movimentos necessários

-- =Função Principal
-- |Função principal
main ::  IO ()
main = do
    m <- openFile "niveiscriados.txt" ReadMode -- O ficheiro com os níveis criados é aberto
    mapas <- hGetContents m -- Os conteúdos são lidos
    re <- openFile "recordes.txt" ReadMode -- O ficheiro com os recordes é aberto
    recordes <- hGetContents re-- Os conteúdos são lidos
    Just ms <- loadJuicy "mainscreen.png" -- As imagens são carregadas
    Just pere <- loadJuicy "pereste.png"
    Just pero <- loadJuicy "peroeste.png"
    Just bl <- loadJuicy "bloco.png"
    Just ca <- loadJuicy "caixa.png"
    Just por <- loadJuicy "vent.png"
    Just es <- loadJuicy "stars.png"
    Just mjo <- loadJuicy "jogar.png"
    Just mni <- loadJuicy "menuniveis.png"
    Just fre <- loadJuicy "melhormov.png"
    Just fno <- loadJuicy "outromov.png"
    Just criamapa <- loadJuicy "instrucoescriadormapa.png"
    Just zero <- loadJuicy "0.png"
    Just um <- loadJuicy "1.png"
    Just dois <- loadJuicy "2.png"
    Just tres <- loadJuicy "3.png"
    Just quatro <- loadJuicy "4.png"
    Just cinco <- loadJuicy "5.png"
    Just seis <- loadJuicy "6.png"
    Just sete <- loadJuicy "7.png"
    Just oito <- loadJuicy "8.png"
    Just nove <- loadJuicy "9.png"
    Just grid <- loadJuicy "grid.png"
    Just escolhermapa <- loadJuicy "procurarmapa.png"
    Just listaerros <- loadJuicy "listaerros.png"
    Just erro1 <- loadJuicy "erro1.png"
    Just erro2 <- loadJuicy "erro2.png"
    Just erro3 <- loadJuicy "erro3.png"
    Just erro4 <- loadJuicy "erro4.png"
    Just erro5 <- loadJuicy "erro5.png"
    let mapasguardados = listamapas [] (lines mapas) -- Os mapas são convertidos de string para mapas
        recordespessoais = carregarecordes (encontrarecordes (reverse (lines recordes)) 14) -- Os recordes são convertidos de string para lista de números
    spawnCommand "mpv --volume=70 --no-video --loop musica.mp3" -- A música é iniciada
    playIO (InWindow "Among Blocos" (1280,720) (0,0)) -- A função de jogo é iniciada
        black
        30
        (criaestado 0 jvazio [ms,pere,pero,bl,ca,por,es,mjo,mni,fre,fno,criamapa,grid,escolhermapa,listaerros] [zero,um,dois,tres,quatro,cinco,seis,sete,oito,nove] [erro1,erro2,erro3,erro4,erro5] 0 1 0 recordespessoais mapasguardados (m,re))
        convestado
        reagevento 
        addtempo