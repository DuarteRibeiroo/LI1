{- |
Module      : Tarefa6_2021li1g038
Description : Resolução de um puzzle
Copyright   : Duarte Afonso Freitas Ribeiro <a100764@alunos.uminho.pt>;
            : António Pedro Cardoso <a100821@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g038 where



import LI12122
import Tarefa1_2021li1g038
import Tarefa2_2021li1g038
import Tarefa3_2021li1g038
import Tarefa4_2021li1g038
import Data.Sort

{- |
Função principal que dado um número máximo de movimentos e um jogo indica, caso seja possível, uma lista de movimentos, 
de comprimento igual ou inferior ao número máximo de movimentos fornecidos, que resolvem o jogo.
-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i (Jogo mapa (Jogador (c,l) d q)) 
    |caminhoPossivel (Jogo mapa (Jogador (c,l) d q)) (c, l) (ondePorta (desconstroiMapa mapa)) == Nothing = Nothing
    |simplifica (caminhoPossivel (Jogo mapa (Jogador (c,l) d q)) (c, l) (ondePorta (desconstroiMapa mapa))) > i = Nothing
    |otherwise = caminhoPossivel (Jogo mapa (Jogador (c,l) d q)) (c, l) (ondePorta (desconstroiMapa mapa))


{- |
Função auxiliar da resolveJogo que dado um jogo e as coordenadas iniciais da personagem, fornece uma lista de movimentos 
que resolvem o jogo, caso a sua resolução seja possível.
-}
caminhoPossivel :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
caminhoPossivel jogo (c, l) (co, lo) -- (c, l) são as coordenadas iniciais da personagem e são utilizadas para saber o acesso inicial a caixas
  |simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireita jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) < simplifica (vCaminhoDireita jogo (c, l) (co, lo)) = vCaminhoEsquerda jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireita jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) >= simplifica (vCaminhoDireita jogo (c, l) (co, lo)) = vCaminhoDireita jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireita jogo (c, l) (co, lo)) == (-1) = vCaminhoEsquerda jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerda jogo (c, l) (co, lo)) == (-1) && simplifica (vCaminhoDireita jogo (c, l) (co, lo)) /= (-1) = vCaminhoDireita jogo (c, l) (co, lo)
  |otherwise = vCaminhosAlternativos jogo (c, l) (co, lo)


{- |
Função auxiliar da função caminhoPossivel, que resolve o jogo, se a resolução for possível, caso a personagem tenha de 
ultrapassar uma queda superior a um bloco, ou a porta não esteja no mesmo "chão" que a personagem e seja preciso subir até 
esse "chão".
-}
vCaminhosAlternativos:: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhosAlternativos jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) < simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) = vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) >= simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) = vCaminhoDireitaAbaixo jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) /= (-1) && simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) == (-1) = vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)
  |simplifica (vCaminhoEsquerdaAbaixo jogo (c, l) (co, lo)) == (-1) && simplifica (vCaminhoDireitaAbaixo jogo (c, l) (co, lo)) /= (-1) = vCaminhoDireitaAbaixo jogo (c, l) (co, lo)
  |otherwise = vCaminhoAcima jogo (c, l) (co, lo)


{- |
Função auxiliar da função vCaminhosAlternativos, que resolve o jogo, se a resolução for possível, nas situações em que a 
porta não se encontra no mesmo "chão" que a personagem e é necessário subir até esse chão.
-}
vCaminhoAcima :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoAcima (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |((simplifica (encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))) /= (-1)) && ((cj - (coluna (last (movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))))>2) && ((simplifica (vCaminhoEsquerdaAcima "se_e" ((last (movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo))) /= (-1)) = vCaminhoEsquerdaAcima "se_e" ((last( movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |((simplifica (encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))) /= (-1)) && ((coluna (head (movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))>2)) && ((simplifica (vCaminhoEsquerdaAcima "se_d" ((head (movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo))) /= (-1)) = vCaminhoEsquerdaAcima "se_d" ((head( movimento(encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |((simplifica (encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))) == (-1)) && simplifica (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)) /= (-1) && (((coluna (head (movimento (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)))))-cj)>2) && ((simplifica (vCaminhoDireitaAcima "sd_d" (head (movimento (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)))/=(-1)) = (vCaminhoDireitaAcima "sd_d" (head (movimento (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo))
  |((simplifica (encontraTeto (cj, lj) (reverse(vChaoEsquerda (desconstroiMapa mapa) (cj, lj))) (desconstroiMapa mapa))) == (-1)) && simplifica (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)) /= (-1) && ((coluna (last (movimento(encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa))))<((largura (desconstroiMapa mapa) cj)-2))) && ((simplifica (vCaminhoDireitaAcima "sd_e" (last (movimento (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)))/=(-1)) = (vCaminhoDireitaAcima "sd_e" (last (movimento (encontraTeto (cj, lj) (vChaoDireita (desconstroiMapa mapa) (cj, lj)) (desconstroiMapa mapa)))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo))
  |otherwise = Nothing



--Funções auxiliares de segundo nível


{-|  
Função que indica, se possível, a lista de  movimentos para alcançar a porta à esquerda da personagem caso a porta se 
encontre no mesmo "chão" que a personagem e a personagem não tenha de cair mais de de um bloco num só movimento.
-}
vCaminhoEsquerda :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |(quantasCaixas (ondeCaixas "ne"  (cj, lj) (co, lo) (vChaoDireita (desconstroiMapa mapa) (co, lo)))) == 0 = esquerdaSimples  (co, lo) (Jogo mapa (Jogador (cj, lj) d q))
  |(quantasCaixas (ondeCaixas "ne"  (cj, lj) (co, lo) (vChaoDireita (desconstroiMapa mapa) (co, lo)))) > 0 = junta (colocaCaixas "ne" (ondeCaixas "ne"  (cj, lj) (co, lo) (vChaoDireita (desconstroiMapa mapa) (co, lo))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)) (esquerdaSimples  (co, lo) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento(colocaCaixas "ne" (ondeCaixas "ne"  (cj, lj) (co, lo) (vChaoDireita (desconstroiMapa mapa) (co, lo))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))
  |otherwise = Nothing

{-|
Função que indica, se possível, a lista de  movimentos para alcançar as coordenadas fornecidas, (cp, lp), à direita da 
personagem caso as mesmas  se encontrem no mesmo "chão" que a personagem e a personagem não tenha de cair mais de de um 
bloco num só movimento.
-}
vCaminhoDireita :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |(quantasCaixas (ondeCaixas "nd"  (cj, lj) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (co, lo)))) == 0 = direitaSimples  (co, lo) (Jogo mapa (Jogador (cj, lj) d q))
  |(quantasCaixas (ondeCaixas "nd"  (cj, lj) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (co, lo)))) > 0 = junta (colocaCaixas "nd" (ondeCaixas "nd"  (cj, lj) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (co, lo))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)) (direitaSimples  (co, lo) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento(colocaCaixas "nd" (ondeCaixas "nd"  (cj, lj) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (co, lo))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))
  |otherwise = Nothing

{- |
Função que indica, se possível, a lista de  movimentos para alcançar a porta à esquerda nos casos em que a personagem tem 
de efetuar uma queda superior a um bloco.
-}
vCaminhoEsquerdaAbaixo :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoEsquerdaAbaixo (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo) = junta (vCaiEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)) (caminhoPossivel (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento(vCaiEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l)  (co, lo)))) (c, l)  (co, lo)) 

{- |
Função que indica, se possível, a lista de  movimentos para alcançar a porta à direita nos casos em que a personagem tem 
de efetuar uma queda superior a um bloco.
-}
vCaminhoDireitaAbaixo :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoDireitaAbaixo (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo) = junta (vCaiDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)) (caminhoPossivel (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento(vCaiDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)))) (c, l)  (co, lo)) 

{-|
Função que indica, se possível, a lista de  movimentos para alcançar a porta à esquerda nos casos em que a porta não se 
encontra no mesmo "chão" que a personagem, sendo portanto necessário subir até esse chão.
-}
vCaminhoEsquerdaAcima :: String -> Coordenadas -> Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoEsquerdaAcima o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo) = junta (vSobeEsquerda o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)) (caminhoPossivel (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (vSobeEsquerda o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)))) (c, l) (co, lo))

{-|
Função que indica, se possível, a lista de  movimentos para alcançar a porta à direita nos casos em que a porta não se 
encontra no mesmo "chão" que a personagem, sendo portanto necessário subir até esse chão.
-}
vCaminhoDireitaAcima :: String -> Coordenadas -> Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaminhoDireitaAcima o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo) = junta (vSobeDireita o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)) (caminhoPossivel (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (vSobeDireita o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)))) (c, l) (co, lo))



-- Funções auxiliares de terceiro nível


{- |
Função que indica, se possível, a lista de  movimentos para alcançar as coordenadas fornecidas, (cp, lp), à esquerda da 
personagem caso as mesmas se encontrem no mesmo "chão" que a personagem e a personagem não tenha de cair mais de de um 
bloco num só movimento e já não seja necessário interagir com caixas.
-}
esquerdaSimples :: Coordenadas -> Jogo -> Maybe [Movimento]
esquerdaSimples (cp, lp) (Jogo mapa (Jogador (c, l) d q))
    |cp==c && lp==l = Just []
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c-1), (l-1))) && (pecaChao (desconstroiMapa mapa) ((c-1), l)) && d == Oeste = (junta (Just [Trepar])  (esquerdaSimples (cp, lp) (Jogo mapa (Jogador ((c-1), (l-1)) d q))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c-1), (l-1))) && (pecaChao (desconstroiMapa mapa) ((c-1), l)) && d == Este = (junta (Just [AndarEsquerda]) (junta (Just [Trepar]) (esquerdaSimples (cp, lp) (Jogo mapa (Jogador ((c-1), (l-1)) d q)))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c-1), (l))) && (pecaChao (desconstroiMapa mapa) ((c-1), (l+1))) = (junta (Just [AndarEsquerda])  (esquerdaSimples (cp, lp) (Jogo mapa (Jogador ((c-1), l) d q))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c-1), (l))) && (vazioChao (desconstroiMapa mapa) ((c-1), (l+1))) && (pecaChao (desconstroiMapa mapa) ((c-1), (l+2))) = (junta (Just [AndarEsquerda]) (esquerdaSimples (cp, lp) (Jogo mapa (Jogador ((c-1), (l+1)) d q))))
    |otherwise = Nothing

{- |
Função que indica, se possível, a lista de  movimentos para alcançar as coordenadas fornecidas, (cp, lp), à direita da 
personagem caso as mesmas se encontrem no mesmo "chão" que a personagem e a personagem não tenha de cair mais de de um 
bloco num só movimento e já não seja necessário interagir com caixas.
-}
direitaSimples :: Coordenadas -> Jogo -> Maybe [Movimento]
direitaSimples (cp, lp) (Jogo mapa (Jogador (c, l) d q))
    |cp==c && lp==l = Just []
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c+1), (l-1))) && (pecaChao (desconstroiMapa mapa) ((c+1), l)) && d == Este = (junta (Just [Trepar]) (direitaSimples (cp, lp) (Jogo mapa (Jogador ((c+1), (l-1)) d q))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c+1), (l-1))) && (pecaChao (desconstroiMapa mapa) ((c+1), l)) && d == Oeste = (junta (Just [AndarDireita]) (junta (Just [Trepar]) (direitaSimples (cp, lp) (Jogo mapa (Jogador ((c+1), (l-1)) d q)))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c+1), (l))) && (pecaChao (desconstroiMapa mapa) ((c+1), (l+1))) = (junta (Just [AndarDireita]) (direitaSimples (cp, lp) (Jogo mapa (Jogador ((c+1), l) d q))))
    |(cp/=c || lp/=l) && (vazioChao (desconstroiMapa mapa) ((c+1), (l))) && (vazioChao (desconstroiMapa mapa) ((c+1), (l+1))) && (pecaChao (desconstroiMapa mapa) ((c+1), (l+2))) = (junta (Just [AndarDireita]) (direitaSimples (cp, lp) (Jogo mapa (Jogador ((c+1), (l+1)) d q))))
    |otherwise = Nothing

{- |  
Função que indica, se possível, a lista de  movimentos para alcançar a (primeira) queda à esquerda da personagem.
-}
vCaiEsquerda :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaiEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |existeQueda (vChaoEsquerda (desconstroiMapa mapa) (cj, lj)) == False = Nothing
  |otherwise = junta (vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (ondeQueda (vChaoEsquerda (desconstroiMapa mapa) (cj, lj)))) (Just [AndarEsquerda])
  

{- |  
Função que indica, se possível, a lista de  movimentos para alcançar a (primeira) queda à direita da personagem.
-}
vCaiDireita :: Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vCaiDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |existeQueda (vChaoDireita (desconstroiMapa mapa) (cj, lj)) == False = Nothing
  |otherwise = junta (vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (ondeQueda (vChaoDireita (desconstroiMapa mapa) (cj, lj)))) (Just [AndarDireita])

{- |  
Função que indica, se possível, a lista de  movimentos para alcançar o (primeiro) chão acima e à esquerda da personagem.
-}
vSobeEsquerda :: String -> Coordenadas -> Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vSobeEsquerda o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |(o=="se_e") && vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs,ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))))==Nothing = Nothing
  |(o=="se_e") = (junta (junta (vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "se_e" (ondeCaixas "se_e" (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l))) (esquerdaSimples (cs, ls) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (junta (vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "se_e" (ondeCaixas "se_e" (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))))
  |(o=="se_d") && vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs,ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))))==Nothing = Nothing
  |(o=="se_d") = (junta (junta (vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "se_d" (ondeCaixas "se_d" (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l))) (direitaSimples (cs, ls) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (junta (vCaminhoEsquerda (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "se_d" (ondeCaixas "se_d" (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))))
  |otherwise = Nothing

{- |  
Função que indica, se possível, a lista de  movimentos para alcançar o (primeiro) chão acima e à direita da personagem.
-}
vSobeDireita :: String -> Coordenadas -> Jogo -> Coordenadas -> Coordenadas -> Maybe [Movimento]
vSobeDireita o (cs, ls) (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (co, lo)
  |(o=="sd_e") && vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls)))) ==Nothing = Nothing
  |(o=="sd_e") = (junta (junta (vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "sd_e" (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l))) (esquerdaSimples (cs, ls) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (junta (vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "sd_e" (ondeCaixas "sd_e" (cj, lj) (cs, ls) (vChaoDireita (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))))
  |(o=="sd_d") && vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls)))) ==Nothing = Nothing
  |(o=="sd_d") = (junta (junta (vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "sd_d" (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l))) (direitaSimples (cs, ls) (correrMovimentos (Jogo mapa (Jogador (cj, lj) d q)) (movimento (junta (vCaminhoDireita (Jogo mapa (Jogador (cj, lj) d q)) (c, l) (last (ondeCaixas o (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))))) (colocaCaixas "sd_d" (ondeCaixas "sd_d" (cj, lj) (cs, ls) (vChaoEsquerda (desconstroiMapa mapa) (cs, ls))) (Jogo mapa (Jogador (cj, lj) d q)) (c, l)))))))
  |otherwise = Nothing

{- |
Função que indica, se possível, a lista de movimentos para colocar todas as caixas necessárias nas coordenadas fornecidas
pela função ondeCaixas.
-}
colocaCaixas :: String -> [Coordenadas] -> Jogo -> Coordenadas -> Maybe [Movimento]
colocaCaixas o caixasn (Jogo mapa (Jogador (cj, lj) d q)) (c, l) = caminhoCaixas o caixasn (desconstroiMapa mapa) (Jogo mapa(Jogador (cj, lj) d q)) (c, l)

{- |
Função auxiliar da colocaCaixas que, indica se possível, a lista de movimentos para colocar todas as caixas necessárias.
-}
caminhoCaixas :: String -> [Coordenadas] -> [(Peca, Coordenadas)] -> Jogo -> Coordenadas -> Maybe [Movimento]
caminhoCaixas o caixasn pCo (Jogo mapa (Jogador (cj, lj) d q)) (c, l)
  |((length (caixasn))==1) && (o=="se_e" || o=="se_d" || o=="sd_e" || o=="sd_d") = caminho_1caixa o caixasn (encontraCaixas pCo 1 (vChaoDireita pCo (head caixasn))) (Jogo mapa (Jogador (cj, lj) d q))
  |((length (caixasn))==1) && (o=="se_e" || o=="se_d" || o=="sd_e" || o=="sd_d") && ((caminho_1caixa o caixasn (encontraCaixas pCo 1 (vChaoDireita pCo (head caixasn))) (Jogo mapa (Jogador (cj, lj) d q)))==Nothing) = caminho_1caixa o caixasn (encontraCaixas pCo 1 (vChaoEsquerda pCo (head caixasn))) (Jogo mapa (Jogador (cj, lj) d q))
  |(o=="ne") = caminhoCaixas_2 "ne" (ordenaCaixas caixasn "ne") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoDireita pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |(o=="qe") = caminhoCaixas_2 "qe" (ordenaCaixas caixasn "qe") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoDireita pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |(o=="nd") = caminhoCaixas_2 "nd" (ordenaCaixas caixasn "nd") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoEsquerda pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |(o=="qd") = caminhoCaixas_2 "qd" (ordenaCaixas caixasn "qd") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoEsquerda pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |((length (caixasn))/=1) && (o=="se_e") = caminhoCaixas_2 "se_e" (ordenaCaixas caixasn "se_e") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoDireita pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |((length (caixasn))/=1) && (o=="se_d") = caminhoCaixas_2 "se_d" (ordenaCaixas caixasn "se_d") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoEsquerda pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |((length (caixasn))/=1) && (o=="sd_e") = caminhoCaixas_2 "sd_e" (ordenaCaixas caixasn "sd_e") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoDireita pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |((length (caixasn))/=1) && (o=="sd_d") = caminhoCaixas_2 "sd_d" (ordenaCaixas caixasn "sd_d") (reverse (encontraCaixas pCo (quantasCaixas caixasn) (vChaoEsquerda pCo (head caixasn)))) (Jogo mapa (Jogador (cj, lj) d q))
  |otherwise = Nothing

{- |
Função auxiliar da caminhoCaixas que indica, se possível, a lista de movimentos para colocar uma caixa, nas situações em
que só é necessária uma caixa e a colocação da mesma permite à personagem aceder a um chão acima do dela.
-}
caminho_1caixa :: String -> [Coordenadas] -> [Coordenadas] -> Jogo -> Maybe [Movimento]
caminho_1caixa o ((cn, ln):[]) ((co, lo):[]) (Jogo mapa (Jogador (cp, lp) d q))
  |(o=="se_e") && (co<cp) && (co>cn) = (junta (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))) (esquerdaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoEsquerda (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))))))
  |(o=="se_e") && (co>cp) = (junta (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))) (esquerdaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoEsquerda (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))))))
  |(o=="se_d") && (co<cn) = (junta (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))) (direitaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoDireita (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))))))
  |(o=="sd_d") && (co>cp) && (co<cn) = (junta (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))) (direitaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoDireita (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))))))
  |(o=="sd_d") && (co<cp) = (junta (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))) (direitaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoDireita (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))))))
  |(o=="sd_e") && (co>cn) = (junta (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))) (esquerdaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoEsquerda (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))))))

{- |
Função auxiliar da caminhoCaixas que indica se possível a lista de movimentos para colocar todas as caixas necessárias.
-}
caminhoCaixas_2 :: String -> [Coordenadas] -> [Coordenadas] -> Jogo -> Maybe [Movimento]
caminhoCaixas_2 _ [] [] _ = Just []
caminhoCaixas_2 _ l [] _= Nothing
caminhoCaixas_2 o ((cn, ln):ns) ((co, lo):os) (Jogo mapa (Jogador (cp, lp) d q))
  |(length ((cn, ln):ns))>(length ((co, lo):os))=Nothing
  |otherwise = (junta (caminhoCaixas_3 o (cn, ln) (co, lo) (Jogo mapa (Jogador (cp, lp) d q))) (caminhoCaixas_2 o ns os (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (caminhoCaixas_3 o (cn, ln) (co, lo) (Jogo mapa (Jogador (cp, lp) d q)))))))

{- |
Função auxiliar da caminhoCaixas que indica se possível a lista de movimentos para colocar todas as caixas necessárias.
-}
caminhoCaixas_3 :: String -> Coordenadas -> Coordenadas -> Jogo -> Maybe [Movimento]
caminhoCaixas_3 o (cn, ln) (co, lo) (Jogo mapa (Jogador (cp, lp) d q))
  |(o=="qe" || (o=="ne") || (o=="se_e") || (o=="se_d")) && (co<cp) = (junta (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))) (esquerdaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoEsquerda (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))))) 
  |(o=="qe" || (o=="ne") || (o=="se_e") || (o=="se_d")) && (co>cp) = (junta (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))) (esquerdaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoEsquerda (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))))) 
  |(o=="qd" || (o=="nd") || (o=="sd_e") || (o=="sd_d")) && (co<cp) = (junta (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))) (direitaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoDireita (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (esquerdaACaixa d (cp, lp) (co, lo) (vChaoEsquerda (desconstroiMapa mapa) (cp, lp))))))))) 
  |(o=="qd" || (o=="nd") || (o=="sd_e") || (o=="sd_d")) && (co>cp) = (junta (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))) (direitaComCaixa d (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp)))))) (cn, ln) (vChaoDireita (desconstroiMapa (selecionaMapa (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))) (ondePersonagem (correrMovimentos (Jogo mapa (Jogador (cp, lp) d q)) (movimento (direitaACaixa d (cp, lp) (co, lo) (vChaoDireita (desconstroiMapa mapa) (cp, lp))))))))) 
  |otherwise = Nothing

{- |
Função que indica, se possível, a lista de movimentos para alcançar as coordenadas à esquerda da personagem onde se 
encontra a caixa que deve ser utilizada.
-} 
esquerdaACaixa :: Direcao -> Coordenadas -> Coordenadas -> [Coordenadas] -> Maybe [Movimento]
esquerdaACaixa _ _ _ [] = Nothing
esquerdaACaixa d (cp, lp) (co, lo) ((c', l'):xs)
  |(cp==(co+1)) && (lp==(lo-1)) = Just [AndarEsquerda, AndarEsquerda, AndarDireita, InterageCaixa]
  |d==Este && (l'>lp)= junta (Just [AndarEsquerda]) (esquerdaACaixa Oeste (c', (l'-1)) (co, lo) xs)
  |d==Este && (lp==l') = junta (Just [AndarEsquerda]) (esquerdaACaixa Oeste (cp, lp) (co,lo) ((c', l'):xs))
  |lp==lo && (cp-1)==co = Just (InterageCaixa:[])
  |lp>(l'+1)  = Nothing
  |lp==l' = junta (Just [Trepar]) (esquerdaACaixa d (c', (l'-1)) (co, lo) xs)
  |otherwise = junta (Just [AndarEsquerda]) (esquerdaACaixa d (c', (l'-1)) (co, lo) xs)

{- |
Função que indica, se possível, a lista de movimentos para alcançar as coordenadas à direita da personagem onde se 
encontra a caixa que deve ser utilizada.
-} 
direitaACaixa :: Direcao -> Coordenadas -> Coordenadas -> [Coordenadas] -> Maybe [Movimento]
direitaACaixa _ _ _ [] = Nothing
direitaACaixa d (cp, lp) (co, lo) ((c', l'):xs)  
  |(cp==(co-1)) && (lp==(lo-1)) = Just [AndarDireita, AndarDireita, AndarEsquerda, InterageCaixa]
  |d==Oeste && (lp<l') = junta (Just [AndarDireita]) (direitaACaixa Este (c', (l'-1)) (co, lo) xs)
  |d==Oeste && (lp==l') = junta (Just [AndarDireita]) (direitaACaixa Este (cp, lp) (co, lo) ((c', l'):xs))
  |lp==lo && (cp+1)==co = Just (InterageCaixa:[])
  |lp>(l'+1)  = Nothing
  |lp==l' = junta (Just [Trepar]) (direitaACaixa d (c', (l'-1)) (co, lo) xs)
  |otherwise = junta (Just [AndarDireita]) (direitaACaixa d (c', (l'-1)) (co, lo) xs)

{- |
Função que indica, se possível, a lista de movimentos para alcançar as coordenadas à esquerda da personagem onde a caixa 
deve ser colocada.
-} 
esquerdaComCaixa :: Direcao -> [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas -> [Coordenadas] -> Maybe [Movimento]
esquerdaComCaixa d pCo  (cp, lp) (cn, ln) ((c', l'):xs)
  |d==Este && (lp<l') && (vazioChao pCo (c', lp)) && (vazioChao pCo (c', (lp-1)))= junta (Just [AndarEsquerda]) (esquerdaComCaixa Oeste pCo (c', (l'-1)) (cn, ln) xs)
  |d==Este && (lp==l')= junta (Just [AndarEsquerda]) (esquerdaComCaixa Oeste pCo (cp, lp) (cn, ln) ((c', l'):xs))
  |(ln+1)>=lp && ((cp-1)==cn) = Just (InterageCaixa:[])
  |(lp==l') && (vazioChao pCo (cp, (lp-2))) && (vazioChao pCo (c', (lp-1))) && (vazioChao pCo (c', (lp-2))) = junta (Just [Trepar]) (esquerdaComCaixa d pCo (c',(l'-1)) (cn, ln) xs)
  |((l'-lp)>=1) && (vazioChao pCo (c', lp)) && (vazioChao pCo (c', (lp-1))) = junta (Just [AndarEsquerda]) (esquerdaComCaixa d pCo (c',(l'-1)) (cn, ln) xs)
  |otherwise = Nothing
  
{- |
Função que indica, se possível, a lista de movimentos para alcançar as coordenadas à direita da personagem onde a caixa 
deve ser colocada.
-}   
direitaComCaixa :: Direcao -> [(Peca,Coordenadas)] -> Coordenadas -> Coordenadas -> [Coordenadas] -> Maybe [Movimento]
direitaComCaixa d pCo (cp, lp) (cn, ln) ((c', l'):xs)
  |d==Oeste && (lp<l') && (vazioChao pCo (c', lp)) && (vazioChao pCo (c', (lp-1)))= junta (Just [AndarDireita]) (direitaComCaixa Este pCo (c', (l'-1)) (cn, ln) xs)
  |d==Oeste && (lp==l')= junta (Just [AndarDireita]) (direitaComCaixa Este pCo (cp, lp) (cn, ln) ((c', l'):xs))
  |(ln+1)>=lp && ((cp+1)==cn) = Just (InterageCaixa:[])
  |(lp==l') && (vazioChao pCo (cp, (lp-2))) && (vazioChao pCo (c', (lp-1))) && (vazioChao pCo (c', (lp-2))) = junta (Just [Trepar]) (direitaComCaixa d pCo (c',(l'-1)) (cn, ln) xs)
  |((l'-lp)>=1) && (vazioChao pCo (c', lp)) && (vazioChao pCo (c', (lp-1))) = junta (Just [AndarDireita]) (direitaComCaixa d pCo (c',(l'-1)) (cn, ln) xs)
  |otherwise = Nothing



--Funções auxiliares de quarto nível


{- |
Função que indica a lista de coordenadas onde é necessário colocar caixas para ser possível alcançar as coordenadas 
fornecidas.
-}
ondeCaixas :: String -> Coordenadas -> Coordenadas -> [Coordenadas] -> [Coordenadas]
ondeCaixas i (cj, lj) (c, l) t = ondeCaixas_2 i (cj, lj) ([(c,(l+1))]++t)
{- i é o tipo de funçao anterior . (c,l) sao as coordenadas a atingir pela personagem atençao que nos 
casos de queda essas coordenadas estao uma coluna a esquerda ou a direita. Em todos os casos a linha é 
uma acima a linha do chao. (x:xs) são as coordenadas do chao a esquerda ou a direita dependendo do caso.
-}

{- |
Função auxiliar da função ondeCaixas que recebe a lista das coordenadas das peças do chão invertida, ou seja, até à 
personagem.
-}
ondeCaixas_2 :: String -> Coordenadas -> [Coordenadas] -> [Coordenadas]
ondeCaixas_2 _ _ [] = []
ondeCaixas_2 i (cj, lj) ((c', l'):xs) = ondeCaixas_3 (c', l') xs (cj,lj)

{- |
Função auxiliar da função ondeCaixas_2 que determina o número de caixas necessárias tendo em conta a diferença de altura
entre peças do chão consecutivas.
-}
ondeCaixas_3 :: Coordenadas -> [Coordenadas] -> Coordenadas -> [Coordenadas]
ondeCaixas_3 _ [] _= []
ondeCaixas_3 (c, l) ((c', l'):xs) (cj,lj)
  |(cj==c) = []
  |(l-l')<(-1) = ((c', (l'-1)):(ondeCaixas_3 (c, l) ((c', (l'-1)):xs) (cj, lj)))  
  |otherwise = ondeCaixas_3 (c', l') xs (cj, lj)

{- |
Função que recebe a lista das coordenadas onde devem ser colocadas as caixas e a transforma numa lista com os mesmos 
elementos mas por uma ordem que favorece a colocação das caixas.
-}
ordenaCaixas :: [Coordenadas] -> String -> [Coordenadas]
ordenaCaixas l s = juntalistas (cortafim (ordenaalturas (ordenalinhas (separaalturas l))) s) s

{- |
Função que encontra as n caixas necessárias mais próximas das coordenadas onde ela são necessárias.
-}
encontraCaixas :: [(Peca, Coordenadas)] -> Int -> [Coordenadas] -> [Coordenadas]
encontraCaixas _ 0 _ = []
encontraCaixas _ _ [] = []
encontraCaixas pCo n (x:xs)
  |((pecaCaixa pCo x)==True) = (x:(encontraCaixas pCo (n-1) xs))
  |otherwise = encontraCaixas pCo n xs

{- |
Função que fornece todas as coordenadas de um dado chão através da junção das coordenadas do chão à esquerda, por baixo, e
à direita, da personagem.
-}
chao :: [(Peca, Coordenadas)] -> Coordenadas -> [Coordenadas]
chao ((p, (c, l)):xs) (c', l') = ((reverse(vChaoEsquerda ((p, (c, l)):xs) (c', l'))) ++ (((c'-1), (l'-1)):(vChaoDireita ((p, (c, l)):xs) (c', l'))))

{- |
Função que indica as coordenadas onde se encontra o bloco superior daquele chão à esquerda da personagem.
-}
vChaoEsquerda :: [(Peca, Coordenadas)] -> Coordenadas -> [Coordenadas]
vChaoEsquerda ((p, (c, l)):xs) (c', l')
  |c' ==(0) = []
  |pecaChao ((p, (c, l)):xs) ((c'-1), (l')) == True && pecaChao ((p, (c, l)):xs) ((c'-1), (l'-1)) == False = [((c'-1), (l'))] ++ vChaoEsquerda ((p, (c, l)):xs) ((c'-1), l')
  |pecaChao ((p, (c, l)):xs) ((c'-1), (l'-1)) == True = vChaoEsquerda ((p, (c, l)):xs) (c', (l'-1))
  |pecaChao ((p, (c, l)):xs) ((c'-1), (l')) == False && pecaChao ((p, (c, l)):xs) ((c'-1), (l'-1)) == False = vChaoEsquerda ((p, (c, l)):xs) (c', (l'+1))

{- |
Função que indica as coordenadas onde se encontra o bloco superior daquele chão à direita da personagem.
-}
vChaoDireita :: [(Peca, Coordenadas)] -> Coordenadas -> [Coordenadas]
vChaoDireita ((p, (c, l)):xs) (c', l')
  |c' == ((largura ((p, (c, l)):xs) 0)) = []
  |pecaChao ((p, (c, l)):xs) ((c'+1), (l')) == True && pecaChao ((p, (c, l)):xs) ((c'+1), (l'-1)) == False = (((c'+1), (l')): vChaoDireita ((p, (c, l)):xs) ((c'+1), l'))
  |pecaChao ((p, (c, l)):xs) ((c'+1), (l'-1)) == True = vChaoDireita ((p, (c, l)):xs) (c', (l'-1))
  |pecaChao ((p, (c, l)):xs) ((c'+1), (l')) == False && pecaChao ((p, (c, l)):xs) ((c'+1), (l'-1)) == False = vChaoDireita ((p, (c, l)):xs) (c', (l'+1))

{- |
Função que encontra um nível de blocos acima da personagem para o qual esta possa possivelmente subir, caso esse nível de
blocos exista
-}
encontraTeto :: Coordenadas -> [Coordenadas] -> [(Peca, Coordenadas)] -> Maybe [Coordenadas]
encontraTeto _ [] _ = Nothing
encontraTeto (cj, lj) ((c,l):t) pCo
  |lj<2 = Nothing
  |l<2 = encontraTeto (cj, lj) t pCo
  |pecaChao pCo (c, (l-1))==False = encontraTeto (cj, lj) ((c,(l-1)):t) pCo 
  |pecaChao pCo (c, (l-1))==True && pecaChao pCo (c, (l-2))==True = encontraTeto (cj, lj) ((c,(l-1)):t) pCo 
  |pecaChao pCo (c, (l-1))==True && pecaChao pCo (c, (l-2))==False = junta (Just [(c, (l-1))]) (Just (vChaoTeto (chao pCo (c, (l-2))) ((c,l):t)))

{- |
Função que compara um possível chão acima da personagem com aquele em que ela se encontra para determinar até onde se 
estende o teto (chão acima da personagem).
-}
vChaoTeto :: [Coordenadas] -> [Coordenadas] -> [Coordenadas]
vChaoTeto [] _ = []
vChaoTeto (x:xs) (y:ys) = ((vChaoTeto_2 x (y:ys))++(vChaoTeto xs (y:ys)))

{- |
Função auxiliar da função vChaoTeto que verifica se uma dada coordenada existe o chão em que a personagem se encontra.
-}
vChaoTeto_2 :: Coordenadas -> [Coordenadas] -> [Coordenadas]
vChaoTeto_2 x [] = [x]
vChaoTeto_2 x (y:ys)
  |x==y = []
  |otherwise = vChaoTeto_2 x ys

{- |
Função que junta as listas da função cortafim pela ordem correta.
-}
juntalistas :: ([[Coordenadas]],[Coordenadas]) -> String -> [Coordenadas]
juntalistas (l,c) s
    |last s == 'e' = (concat l) ++ c
    |otherwise = (concat (reverse l)) ++ c

{- |
Função que separa as caixas em listas de acordo com a sua altura.
-}
separaalturas :: [Coordenadas] -> [[Coordenadas]]
separaalturas [] = []
separaalturas ((x,y):xs) = (filter (ligual (x,y)) ((x,y):xs)):(separaalturas (filter (ldif (x,y)) xs))
 
{- |
Função que ordena as caixas de uma dada linha.
-}
ordenalinhas :: [[Coordenadas]] -> [[Coordenadas]]
ordenalinhas (l:ls)= map (sortBy comparax) (l:ls)

{- |
Função que ordena as listas de acordo com a sua altura. As que têm uma posição inferior no mapa aparecem primeiro.
-}
ordenaalturas ::[[Coordenadas]] -> [[Coordenadas]]
ordenaalturas l = reverse (sortBy comparay l)

{- |
Função que remove a última caixa de cada linha de caixas.
-}
cortafim :: [[Coordenadas]] -> String -> ([[Coordenadas]],[Coordenadas])
cortafim l s
    |last s == 'e' = (map init l,map last l)
    |otherwise = (map tail l, map head l)



--Funções auxiliares de quinto nível


{- |
Função que indica as coordenadas da personagem.
-}
ondePersonagem :: Jogo -> Coordenadas
ondePersonagem (Jogo mapa (Jogador co d q)) = co

{- |
Função que seleciona o mapa de um jogo.
-}
selecionaMapa :: Jogo -> [[Peca]]
selecionaMapa (Jogo mapa (Jogador co d q)) = mapa

{- |
Função que indica as coordenadas onde se encontra a porta.
-}
ondePorta :: [(Peca, Coordenadas)] -> Coordenadas
ondePorta ((p, co):xs)
    |p==Porta = co
    |otherwise = ondePorta xs

{- |
Função que fornece as coordenadas que a personagem deve atingir para iniciar uma queda.
-}
ondeQueda :: [(Coordenadas)] -> Coordenadas
ondeQueda ((c, l):((c', l'):xs))
  |(l'-l)>1 = (c, (l-1))
  |otherwise = ondeQueda ((c', l'):xs)

{- |
Função que fornece as coordenadas que a personagem deve atingir para completar uma subida. (Uma linha acima do ponto de 
subida.)
-}
aposSubida :: Coordenadas -> Coordenadas
aposSubida (c, l) = (c, (l-1))



-- Funções auxiliares de sexto nível


{- |
Função que calcula a largura de um mapa.
-}
largura :: [(Peca, Coordenadas)] -> Int -> Int
largura [] n = n
largura ((peca, (c, l)):xs) n
  |c>n = largura xs c
  |otherwise = largura xs n

{- |
Função que indica o número de caixas necessárias para alcançar as coordenadas fornecidas.
-}
quantasCaixas :: [Coordenadas] -> Int
quantasCaixas lc = length lc

{- |
Função que recebe a lista de pares de peças e respetivas coordenadas, e uma coordenada e verifica se a personagem se pode
movimentar para essa coordenada.
-}
vazioChao :: [(Peca, Coordenadas)] -> Coordenadas -> Bool
vazioChao [] _ = False
vazioChao ((peca, (c, l)): xs) (c', l')
    |c==c'&& l==l' && peca==Vazio = True
    |c==c'&& l==l' && peca==Porta = True
    |c/=c' = vazioChao xs (c', l')
    |l/=l' = vazioChao xs (c', l')
    |otherwise = False

{- |
Função que recebe a lista de pares de peças e respetivas coordenadas, e uma coordenada e verifica se nessa coordenada
existe uma peça sobre a qual a personagem se possa movimentar.
-}
pecaChao :: [(Peca, Coordenadas)] -> Coordenadas -> Bool
pecaChao [] _ = False
pecaChao ((peca, (c, l)): xs) (c', l')
    |c==c'&& l==l' && peca==Caixa = True
    |c==c' && l==l' && peca==Bloco = True
    |c/=c' = pecaChao xs (c', l')
    |l/=l' = pecaChao xs (c', l')
    |otherwise = False

{- |
Função que recebe a lista de pares de peças e respetivas coordenadas, e uma coordenada e verifica se nessa coordenada
existe uma caixa.
-}
pecaCaixa :: [(Peca, Coordenadas)] -> Coordenadas -> Bool
pecaCaixa [] _ = False
pecaCaixa ((peca, (c, l)): xs) (c', l')
    |peca/=Caixa = pecaCaixa xs (c', l')
    |c==c'&& l==l' && peca==Caixa = True
    |c/=c' = pecaCaixa xs (c', l')
    |l/=l' = pecaCaixa xs (c', l')
    |otherwise = False

{- |
Função que recebe um chão, ou parte dele, e verifica se existe uma queda no mesmo.
-}
existeQueda :: [(Coordenadas)] -> Bool
existeQueda [(c,l)] = False
existeQueda ((c, l):((c', l'):xs))
  |(l'-l)>1 = True
  |otherwise = existeQueda ((c', l'):xs)

{- |
Função que indica a coluna de uma dada coordenada.
-}
coluna :: Coordenadas -> Int
coluna (c, l) = c

{- |
Função que indica a linha de uma dada coordenada.
-}
linha :: Coordenadas -> Int
linha (c, l) = l

{- |
Função que indica se as linhas de duas dadas coordenadas são iguais.
-}
ligual :: Coordenadas -> Coordenadas -> Bool
ligual (c,l) (c2,l2)
    |l==l2 = True
    |otherwise = False

{- |
Função que indica se as linhas de duas dadas coordenadas são diferentes.
-}
ldif :: Coordenadas -> Coordenadas -> Bool
ldif (c,l) (c2,l2)
    |l==l2 = False
    |otherwise = True

{- |
Função que compara as linhas de duas dadas coordenadas.
-}
comparay :: [Coordenadas] -> [Coordenadas] -> Ordering
comparay ((x,y):ls) ((x2,y2):xs) = compare y y2

{- |
Função que compara as colunas de duas dadas coordenadas.
-}
comparax :: Coordenadas -> Coordenadas -> Ordering
comparax (x,y) (x2,y2) = compare x x2



-- Funções auxiliares que facilitam o trabalho com o tipo Maybe [Movimento] e Maybe [a].


{- |
Função que permite comparar quando necessário o comprimento de uma possível lista.
-}
simplifica :: Maybe [a] -> Int
simplifica Nothing = -1 
simplifica (Just a) = length a 

{- |
Função que permite nas condições em que é utilizada a junção de duas possíveis listas de movimentos numa só.
Como ambas as listas são necessárias à realização do movimento completo caso um dos movimentos seja impossível, também
o movimento completo será impossível.
-}
junta :: Maybe [a] -> Maybe [a] -> Maybe [a]
junta Nothing _ = Nothing
junta _ Nothing = Nothing
junta (Just a) (Just b) = Just (a++b)

{- |
Função que transforma uma possível lista , no caso dela existir, numa lista.
-}
movimento :: Maybe [a] -> [a]
movimento (Just a) = a