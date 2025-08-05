module Logica.Bot
  ( escolherNaviosBot
  , realizarJogadaBot
  ) where

import Jogo.Arquitetura
import Logica.Posicionamento
import System.Random.Stateful (uniformRM, globalStdGen)

-- Posiciona todos os navios do bot no tabuleiro, retornando lista de navios atualizados
escolherNaviosBot :: IO [Navio]
escolherNaviosBot = posicionarTodos naviosPadrao criacaoTabuleiro []
  where
    posicionarTodos [] _ acc = return acc
    posicionarTodos (n:ns) tab acc = do
      (navioPosicionado, tabAtualizado) <- posicionarNavio tab n
      posicionarTodos ns tabAtualizado (acc ++ [navioPosicionado])


-- Escolhe uma jogada para o bot considerando tiros já dados (retorna nova coordenada)
realizarJogadaBot :: [Coordenada] -> IO Coordenada
realizarJogadaBot tirosDados = do
  let todas = [(x,y) | x <- [0..tamanhoTabuleiro-1], y <- [0..tamanhoTabuleiro-1]]
      restantes = filter (`notElem` tirosDados) todas
  if null restantes
    then error "Sem posições restantes para jogar"
    else do
      idx <- uniformRM (0, length restantes - 1) globalStdGen
      return (restantes !! idx)
