module Logica.Posicionamento (
    geraCorpoEmbarcacao, 
    validaPosicionamento,
    marcaNaviosNoTabuleiro,
    posicionarNavio,
) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as Tabuleiro
import Logica.Geradores (geraCoordenada, geraOrientacao)

-- Verifica se a coordenada está dentro dos limites do tabuleiro (índices 0 a 9)
coordenadaValida :: Coordenada -> Bool
coordenadaValida (x, y) = x >= 0 && y >= 0 && x < tamanhoTabuleiro && y < tamanhoTabuleiro

-- Gera a lista de coordenadas que o navio ocupa com base na orientação
geraCorpoEmbarcacao :: Coordenada -> Navio -> Orientacao -> [Coordenada]
geraCorpoEmbarcacao (x, y) navio orient =
  let tam = tamanho navio
  in case orient of
       H -> [(x + i, y) | i <- [0..tam - 1]]
       V -> [(x, y + i) | i <- [0..tam - 1]]

-- Verifica se é possível posicionar o navio na posição/orientação dada
validaPosicionamento :: Tabuleiro -> Coordenada -> Navio -> Orientacao -> Bool
validaPosicionamento tab coord navio orient =
  let corpo = geraCorpoEmbarcacao coord navio orient
      dentroLimite = all coordenadaValida corpo
      -- Verifica se todas as células estão livres
      celulasLivres = all (\c -> Tabuleiro.obter c tab == Just Agua) corpo
  in dentroLimite && celulasLivres

-- Marca as coordenadas ocupadas pelo navio como ParteNavio
marcaNaviosNoTabuleiro :: Tabuleiro -> [Coordenada] -> Tabuleiro
marcaNaviosNoTabuleiro = foldl (\tab coord -> Tabuleiro.marca coord ParteNavio tab)

-- Posiciona um navio válido no tabuleiro, repetindo até obter sucesso
posicionarNavio :: Tabuleiro -> Navio -> IO (Navio, Tabuleiro)
posicionarNavio tab navio = do
  pos <- geraCoordenada
  orient <- geraOrientacao
  if validaPosicionamento tab pos navio orient
    then do
      let corpoNavio = geraCorpoEmbarcacao pos navio orient
          novoTab = marcaNaviosNoTabuleiro tab corpoNavio
          novoNavio = navio { posicoes = corpoNavio }
      return (novoNavio, novoTab)
    else posicionarNavio tab navio