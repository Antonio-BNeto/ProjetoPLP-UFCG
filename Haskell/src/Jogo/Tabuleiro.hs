module Jogo.Tabuleiro (marca, obter, obterEstado) where

import Jogo.Arquitetura -- Alterado aqui
import Util.Lista (atualizaIndice)

-- Nova função necessária para o Combate
obterEstado :: Coordenada -> Tabuleiro -> Maybe Celula
obterEstado = obter

obter :: Coordenada -> Tabuleiro -> Maybe Celula
obter (x,y) tab =
  if x >= 0 && y >= 0 && x < length tab && y < length (tab !! x)
     then Just ((tab !! x) !! y)
     else Nothing

marca :: Coordenada -> Celula -> Tabuleiro -> Tabuleiro
marca (x,y) novaCelula tab =
  if x >= 0 && y >= 0 && x < length tab && y < length (tab !! x)
     then
       let linha     = tab !! x
           novaLinha = atualizaIndice y novaCelula linha
       in  atualizaIndice x novaLinha tab
     else tab  -- Retorna o tabuleiro inalterado
