module Logica.Geradores (
  geraCoordenada,
  geraOrientacao
) where

import Jogo.Arquitetura
import System.Random.Stateful (uniformRM, globalStdGen)

geraCoordenada :: IO Coordenada
geraCoordenada = do
  x <- uniformRM (1 :: Int, tamanhoTabuleiro) globalStdGen
  y <- uniformRM (1 :: Int, tamanhoTabuleiro) globalStdGen
  return (x, y)
geraOrientacao :: IO Orientacao
geraOrientacao = do
  n <- uniformRM (0 :: Int, 1 :: Int) globalStdGen  -- forçar tipo Int nos limites
  return $ if n == 0 then H else V
