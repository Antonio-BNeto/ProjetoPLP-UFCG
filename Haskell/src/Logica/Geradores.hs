module Logica.Geradores (geraCoordenada, geraOrientacao) where

import Jogo.Arquitetura
import System.Random.Stateful (uniformRM, globalStdGen)

geraCoordenada :: IO Coordenada
geraCoordenada = do
  x <- uniformRM (0, tamanhoTabuleiro - 1) globalStdGen
  y <- uniformRM (0, tamanhoTabuleiro - 1) globalStdGen
  return (x, y)

geraOrientacao :: IO Orientacao
geraOrientacao = do
  isH <- uniformRM (False, True) globalStdGen
  if isH then return H else return V