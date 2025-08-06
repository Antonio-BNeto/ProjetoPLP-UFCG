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
      case Navio.encontraNavio coordenada listaDeNavios of
        Just _ ->
          let novoTabuleiro     = Tabuleiro.marca coordenada Atingido tabuleiro
              naviosAtualizados = Navio.atualizaNavios coordenada listaDeNavios
          in case Navio.encontraNavio coordenada naviosAtualizados of
              Just navioAtualizado ->
                if Navio.navioAfundado navioAtualizado
                  then (novoTabuleiro, naviosAtualizados, Afundou (tipo navioAtualizado))
                  else (novoTabuleiro, naviosAtualizados, Acertou (tipo navioAtualizado))
              Nothing ->
                (novoTabuleiro, naviosAtualizados, AcertoRepetido)
        Nothing ->

          let novoTabuleiro     = Tabuleiro.marca coordenada Atingido tabuleiro
              naviosAtualizados = Navio.atualizaNavios coordenada listaDeNavios
          in (novoTabuleiro, naviosAtualizados, AcertoRepetido)

    Just Atingido ->
      (tabuleiro, listaDeNavios, AcertoRepetido)

    Just Erro ->
      (tabuleiro, listaDeNavios, ErroRepetido)

verificaVitoria :: [Navio] -> Bool
verificaVitoria = all Navio.navioAfundado
