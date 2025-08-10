module Logica.Posicionamento (
    geraCorpoEmbarcacao, 
    validaPosicionamento,
    marcaNaviosNoTabuleiro,
    atualizaCelula,
    posicionarNavio,
) where

import Logica.Geradores (geraCoordenada, geraOrientacao)
import Jogo.Arquitetura (Coordenada, Tabuleiro, Celula(..), Navio(..), Orientacao(H, V))
import Jogo.Tabuleiro

-- Verifica se a coordenada está dentro dos limites do tabuleiro
coordenadaValida :: Coordenada -> Bool
coordenadaValida (x, y) = x >= 1 && y >= 1 && x <= 10 && y <= 10

-- Gera a lista de coordenadas que o navio ocupa com base na orientação
geraCorpoEmbarcacao :: Coordenada -> Navio -> Orientacao -> [Coordenada]
geraCorpoEmbarcacao (x, y) navio orient =
  let tam = tamanho navio
  in case orient of
       H -> [(x+i, y) | i <- [0..tam - 1]]
       V -> [(x, y+i) | i <- [0..tam - 1]]

-- Verifica se é possível posicionar o navio na posição/orientação dada
validaPosicionamento :: Tabuleiro -> Coordenada -> Navio -> Orientacao -> Bool
validaPosicionamento tab coord navio orient =
  let corpo = geraCorpoEmbarcacao coord navio orient
      dentroLimite = all coordenadaValida corpo
      celulasLivres = all (\(x, y) -> tab !! (x-1) !! (y-1) == Agua) corpo
  in dentroLimite && celulasLivres

-- Atualiza uma célula (x,y) no tabuleiro com um valor dado
atualizaCelula :: Tabuleiro -> Int -> Int -> Celula -> Tabuleiro
atualizaCelula tab x y val = case splitAt y tab of
  (beforeRows, row:afterRows) -> case splitAt x row of
    (beforeCells, _:afterCells) ->
      let newRow = beforeCells ++ [val] ++ afterCells
      in beforeRows ++ [newRow] ++ afterRows
    _ -> tab  -- Fallback seguro
  _ -> tab  -- Fallback seguro

-- Marca as coordenadas ocupadas pelo navio como ParteNavio
marcaNaviosNoTabuleiro :: Tabuleiro -> [Coordenada] -> Tabuleiro
marcaNaviosNoTabuleiro
  = foldl
      (\ t (x, y)
         -> let coord = (x - 1, y - 1)
            in
              case obter coord t of
                Just _ -> marca coord ParteNavio t
                Nothing -> t)

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
