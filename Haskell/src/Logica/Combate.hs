module Logica.Combate (ResultadoAtaque(..), realizarAtaque, verificaVitoria) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as Tabuleiro
import qualified Jogo.Navio     as Navio

data ResultadoAtaque
  = TiroFora              -- O ataque atingiu a água.
  | AcertoRepetido        -- O ataque foi em uma posição de navio já atingida.
  | ErroRepetido          -- O ataque foi em uma posição de água já atacada.
  | Acertou String        -- O ataque acertou um navio, mas não o afundou.
  | Afundou String        -- O ataque acertou e afundou o navio.
  | CoordenadaInvalida    -- A coordenada está fora dos limites do tabuleiro.
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
        Just navio ->
          let novoTabuleiro     = Tabuleiro.marca coordenada Atingido tabuleiro
              naviosAtualizados = Navio.atualizaNavios coordenada listaDeNavios
          in if Navio.navioAfundado navio
               then (novoTabuleiro, naviosAtualizados, Afundou (tipo navio))
               else (novoTabuleiro, naviosAtualizados, Acertou (tipo navio))

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
