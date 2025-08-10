{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Logica.Bot
  ( escolherNaviosBot
  , realizarJogadaBot
  ) where

import Jogo.Arquitetura
import Logica.Posicionamento
import System.Random.Stateful (uniformRM, globalStdGen)
import qualified Logica.Combate as C

-- Posiciona todos os navios do bot no tabuleiro, retornando lista de navios atualizados
escolherNaviosBot :: IO [Navio]
escolherNaviosBot = posicionarTodos naviosPadrao criacaoTabuleiro []
  where
    posicionarTodos [] _ acc = return acc
    posicionarTodos (n:ns) tab acc = do
      (navioPosicionado, tabAtualizado) <- posicionarNavio tab n
      posicionarTodos ns tabAtualizado (acc ++ [navioPosicionado])

-- Função principal do bot para escolher a jogada
realizarJogadaBot :: [Coordenada] -> [C.ResultadoAtaque] -> IO Coordenada
realizarJogadaBot tirosDados resultados =
  case (resultados, tirosDados) of
    -- Dois acertos consecutivos (mais recente r1, anterior r2)
    (C.Acertou _:C.Acertou _:_, c1:c2:_) | mesmoEixo c2 c1 ->
        seguirEixo c2 c1 tirosDados
    (C.Afundou _:C.Acertou _:_, c1:c2:_) | mesmoEixo c2 c1 ->
        seguirEixo c2 c1 tirosDados
    (C.Acertou _:C.Afundou _:_, c1:c2:_) | mesmoEixo c2 c1 ->
        seguirEixo c2 c1 tirosDados

    -- Apenas um acerto recente -> tentar vizinho da última coordenada
    (C.Acertou _:_, coord:_) ->
        jogarVizinho coord tirosDados
    (C.Afundou _:_, coord:_) ->
        jogarVizinho coord tirosDados

    -- Caso contrário -> padrão de caça otimizado
    _ ->
        jogarAleatorioComPadrao tirosDados

-- Segue a direção já confirmada até afundar o navio
seguirEixo :: Coordenada -> Coordenada -> [Coordenada] -> IO Coordenada
seguirEixo (x1,y1) (x2,y2) tirosDados
  | x1 == x2  = seguirDirecao (0, signum (y2 - y1)) (x2, y2) tirosDados
  | y1 == y2  = seguirDirecao (signum (x2 - x1), 0) (x2, y2) tirosDados
  | otherwise = jogarAleatorioComPadrao tirosDados

-- Continua tentando na direção especificada
seguirDirecao :: (Int, Int) -> Coordenada -> [Coordenada] -> IO Coordenada
seguirDirecao (dx,dy) (x,y) tirosDados =
  let proxima = (x + dx, y + dy)
  in if dentroDoTabuleiro proxima && proxima `notElem` tirosDados
        then return proxima
        else jogarAleatorioComPadrao tirosDados

-- Escolhe uma coordenada vizinha válida e não atacada
jogarVizinho :: Coordenada -> [Coordenada] -> IO Coordenada
jogarVizinho (x, y) tirosDados = do
  let vizinhos = filter dentroDoTabuleiro [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
      naoTentados = filter (`notElem` tirosDados) vizinhos
  if null naoTentados
    then jogarAleatorioComPadrao tirosDados
    else do
      idx <- uniformRM (0, length naoTentados - 1) globalStdGen
      return (naoTentados !! idx)

-- Escolhe aleatoriamente uma coordenada válida seguindo padrão de caça
jogarAleatorioComPadrao :: [Coordenada] -> IO Coordenada
jogarAleatorioComPadrao tirosDados = do
  let todas = [(x,y)
              | x <- [0..tamanhoTabuleiro-1]
              , y <- [0..tamanhoTabuleiro-1]
              , (x + y) `mod` 2 == 0]  -- padrão de xadrez
      restantes = filter (`notElem` tirosDados) todas
  if null restantes
    then jogarAleatorio tirosDados  -- fallback caso acabe o padrão
    else do
      idx <- uniformRM (0, length restantes - 1) globalStdGen
      return (restantes !! idx)

-- Escolhe aleatoriamente uma coordenada ainda não atacada no tabuleiro
jogarAleatorio :: [Coordenada] -> IO Coordenada
jogarAleatorio tirosDados = do
  let todas = [(x,y) | x <- [0..tamanhoTabuleiro-1], y <- [0..tamanhoTabuleiro-1]]
      restantes = filter (`notElem` tirosDados) todas
  if null restantes
    then error "Sem posições restantes para jogar"
    else do
      idx <- uniformRM (0, length restantes - 1) globalStdGen
      return (restantes !! idx)

-- ========================================
-- Utilitários
-- ========================================

-- Verifica se duas coordenadas estão no mesmo eixo
mesmoEixo :: Coordenada -> Coordenada -> Bool
mesmoEixo (x1,y1) (x2,y2) = x1 == x2 || y1 == y2

-- Verifica se a coordenada está dentro do tabuleiro
dentroDoTabuleiro :: Coordenada -> Bool
dentroDoTabuleiro (x, y) =
  x >= 0 && x < tamanhoTabuleiro &&
  y >= 0 && y < tamanhoTabuleiro