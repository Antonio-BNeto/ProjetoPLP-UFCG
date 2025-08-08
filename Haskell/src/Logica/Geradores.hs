module Logica.Geradores (
  geraCoordenada,
  geraOrientacao
) where

import Jogo.Arquitetura
import System.Random.Stateful (uniformRM, globalStdGen)

geraCoordenada :: IO Coordenada
geraCoordenada = do
  x <- uniformRM (0 :: Int, tamanhoTabuleiro - 1) globalStdGen
  y <- uniformRM (0 :: Int, tamanhoTabuleiro - 1) globalStdGen
  return (x, y)

geraOrientacao :: IO Orientacao
geraOrientacao = do
  n <- uniformRM (0 :: Int, 1 :: Int) globalStdGen
  return $ if n == 0 then H else V