module Logica.Combate (ResultadoAtaque(..), realizarAtaque, verificaVitoria) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as Tabuleiro
import qualified Jogo.Navio as Navio

data ResultadoAtaque
  = TiroFora
  | AcertoRepetido
  | ErroRepetido
  | Acertou String
  | Afundou String
  | CoordenadaInvalida
  deriving (Eq, Show)

realizarAtaque
  :: Tabuleiro -> [Navio] -> Coordenada
  -> (Tabuleiro, [Navio], ResultadoAtaque)
realizarAtaque tab navios coord =
  case Tabuleiro.obter coord tab of
    Nothing ->
      (tab, navios, CoordenadaInvalida)

    Just Agua ->
      let novoTab = Tabuleiro.marca coord Erro tab
      in (novoTab, navios, TiroFora)

    Just ParteNavio ->
      let novoTab = Tabuleiro.marca coord Atingido tab
          naviosAtualizados = Navio.atualizaNavios coord navios
      in case Navio.encontraNavio coord naviosAtualizados of
           Just navio ->
             if Navio.navioAfundado navio
                then (novoTab, naviosAtualizados, Afundou (tipo navio))
                else (novoTab, naviosAtualizados, Acertou (tipo navio))
           Nothing ->
             (novoTab, naviosAtualizados, AcertoRepetido)

    Just Atingido ->
      (tab, navios, AcertoRepetido)

    Just Erro ->
      (tab, navios, ErroRepetido)

verificaVitoria :: [Navio] -> Bool
verificaVitoria = all Navio.navioAfundado