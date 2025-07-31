module Logica.Combate (ResultadoAtaque(..), realizarAtaque, verificaVitoria) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as T
import qualified Jogo.Navio     as N

data ResultadoAtaque
  = TiroFora
  | TiroRepetidoHit
  | TiroRepetidoMiss
  | Acertou String
  | Afundou String
  | CoordenadaInvalida
  | ErroInterno
  deriving (Eq, Show)

realizarAtaque
  :: Tabuleiro -> [Navio] -> Coordenada
  -> (Tabuleiro, [Navio], ResultadoAtaque)
realizarAtaque tab navios coord =
  case T.obter coord tab of
    Nothing         -> (tab, navios, CoordenadaInvalida)
    Just Agua       ->
      let novoTab = T.marca coord Erro tab
      in  (novoTab, navios, TiroFora)
    Just ParteNavio ->
      let novoTab           = T.marca coord Atingido tab
          naviosAtualizados = N.atualizaNavios coord navios
      in  case N.encontraNavio coord naviosAtualizados of
            Just n  ->
              if N.navioAfundado n
                 then (novoTab, naviosAtualizados, Afundou (tipo n))
                 else (novoTab, naviosAtualizados, Acertou (tipo n))
            Nothing -> (tab, navios, ErroInterno)
    Just Atingido   -> (tab, navios, TiroRepetidoHit)
    Just Erro       -> (tab, navios, TiroRepetidoMiss)

verificaVitoria :: [Navio] -> Bool
verificaVitoria = all N.navioAfundado