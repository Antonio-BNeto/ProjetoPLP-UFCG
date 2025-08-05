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
  | CoordenadaInvalida    -- A coordenada do ataque está fora dos limites do tabuleiro.
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
            -- Se o navio foi atingido no tabuleiro mas não encontrado na lista,
            -- retorna o estado atualizado mas considera como um tiro repetido
            -- para não travar o jogo. Isso aponta para um erro na lógica de setup.
            (novoTabuleiro, naviosAtualizados, AcertoRepetido)

    Just Atingido ->
      (tabuleiro, listaDeNavios, AcertoRepetido)

    Just Erro ->
      (tabuleiro, listaDeNavios, ErroRepetido)

verificaVitoria :: [Navio] -> Bool
verificaVitoria = all Navio.navioAfundado