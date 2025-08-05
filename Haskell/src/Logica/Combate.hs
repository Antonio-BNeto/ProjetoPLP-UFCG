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
  deriving (Eq, Show)

-- realizarAtaque tab navios coord = do
--   1. Verifica célula no tabuleiro (T.obter)
--   2. Se for ParteNavio:
--      a. Atualiza tabuleiro (marca como Atingido)
--      b. Atualiza lista de navios (N.atualizaNavios)
--      c. Busca navio correspondente (N.encontraNavio)
--         - Se encontrar: verifica se afundou
--         - Se não encontrar: trata como tiro repetido (Gambiarra)
--   3. Retorna (Tabuleiro, [Navio], Resultado) atualizados

realizarAtaque :: Tabuleiro -> [Navio] -> Coordenada -> (Tabuleiro, [Navio], ResultadoAtaque)
realizarAtaque tab navios coord =
  case T.obter coord tab of
    Nothing -> (tab, navios, CoordenadaInvalida)
    Just Agua ->
      let novoTab = T.marca coord Erro tab
      in (novoTab, navios, TiroFora)
    Just ParteNavio ->
      let novoTab = T.marca coord Atingido tab
          naviosAtualizados = N.atualizaNavios coord navios
      in case N.encontraNavio coord naviosAtualizados of
           Just navio ->
             if N.navioAfundado navio
                then (novoTab, naviosAtualizados, Afundou (tipo navio))
                else (novoTab, naviosAtualizados, Acertou (tipo navio))
           Nothing -> (novoTab, naviosAtualizados, TiroRepetidoHit) -- Aqui: Mantém naviosAtualizados!
    Just Atingido -> (tab, navios, TiroRepetidoHit)
    Just Erro -> (tab, navios, TiroRepetidoMiss)


verificaVitoria :: [Navio] -> Bool
verificaVitoria = all N.navioAfundado