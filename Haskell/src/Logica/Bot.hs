module Logica.Bot
  ( escolherNaviosBot
  , realizarJogadaBot
  ) where

import Jogo.Arquitetura
import Logica.Posicionamento
import System.Random.Stateful (uniformRM, globalStdGen)
import qualified Data.List as L

-- Posiciona todos os navios do bot no tabuleiro, retornando a lista de navios e o tabuleiro
escolherNaviosBot :: IO ([Navio], Tabuleiro)
escolherNaviosBot = posicionarTodos naviosPadrao criacaoTabuleiro []
  where
    posicionarTodos :: [Navio] -> Tabuleiro -> [Navio] -> IO ([Navio], Tabuleiro)
    posicionarTodos [] tab acc = return (acc, tab)
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
      return (L.head (L.drop idx restantes))