module Jogo.Tabuleiro (marca, obter) where

import Jogo.Arquitetura -- Alterado aqui
import Util.Lista (atualizaIndice)

obter :: Coordenada -> Tabuleiro -> Maybe Celula
obter (x,y) tab =
  if x >= 0 && y >= 0 && x < length tab && y < length (tab !! x)
     then Just ((tab !! x) !! y)
     else Nothing

marca :: Coordenada -> Celula -> Tabuleiro -> Tabuleiro
marca (x,y) novo tab =
  let linha     = tab !! x
      novaLinha = atualizaIndice y novo linha
  in  atualizaIndice x novaLinha tab