module Logica.Combate (ResultadoAtaque(..), realizarAtaque, verificaVitoria) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as Tabuleiro
import qualified Jogo.Navio     as Navio

data ResultadoAtaque
  = TiroFora
  | AcertoRepetido
  | ErroRepetido
  | Acertou String
  | Afundou String
  | CoordenadaInvalida
  | ErroInterno
  deriving (Eq, Show)

realizarAtaque
  :: Tabuleiro -> [Navio] -> Coordenada
  -> (Tabuleiro, [Navio], ResultadoAtaque)
realizarAtaque tabuleiro listaDeNavios coordenada =
  case Tabuleiro.obter coordenada tabuleiro of
    Nothing ->
      (tabuleiro, listaDeNavios, CoordenadaInvalida)

    Just Agua ->
      let novoTabuleiro = Tabuleiro.marca coordenada Erro tabuleiro
      in (novoTabuleiro, listaDeNavios, TiroFora)

    Just ParteNavio ->
      let novoTabuleiro     = Tabuleiro.marca coordenada Atingido tabuleiro
          naviosAtualizados = Navio.atualizaNavios coordenada listaDeNavios
      in
        case Navio.encontraNavio coordenada naviosAtualizados of
          Just navio ->
            if Navio.navioAfundado navio
              then (novoTabuleiro, naviosAtualizados, Afundou (tipo navio))
              else (novoTabuleiro, naviosAtualizados, Acertou (tipo navio))
          Nothing ->
            (tabuleiro, listaDeNavios, ErroInterno)

    Just Atingido ->
      (tabuleiro, listaDeNavios, AcertoRepetido)

    Just Erro ->
      (tabuleiro, listaDeNavios, ErroRepetido)

verificaVitoria :: [Navio] -> Bool
verificaVitoria = all Navio.navioAfundado