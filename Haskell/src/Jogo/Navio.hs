module Jogo.Navio (encontraNavio, atualizaNavios, navioAfundado) where

import Jogo.Arquitetura

encontraNavio :: Coordenada -> [Navio] -> Maybe Navio
encontraNavio _ []     = Nothing
encontraNavio coord (n:ns)
  | coord `elem` posicoes n = Just n
  | otherwise               = encontraNavio coord ns

atualizaNavios :: Coordenada -> [Navio] -> [Navio]
atualizaNavios _     [] = []
atualizaNavios coord (n:ns)
  | coord `elem` posicoes n =
      let jaAtingido = coord `elem` partesAtingidas n
          novasPartes = if jaAtingido then partesAtingidas n else coord : partesAtingidas n
      in n { partesAtingidas = novasPartes } : ns
  | otherwise = n : atualizaNavios coord ns


navioAfundado :: Navio -> Bool
navioAfundado n = all (`elem` partesAtingidas n) (posicoes n)