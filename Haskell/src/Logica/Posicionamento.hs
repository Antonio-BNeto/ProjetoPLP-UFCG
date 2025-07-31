module Logica.Posicionamento (
    geraCorpoEmbarcacao, 
    validaPosicionamento,
    marcaNaviosNoTabuleiro,
    atualizaCelula,
    posicionarNavio
) where
    
import Jogo.Arquitetura

coordenadaValida :: Coordenada -> Bool
coordenadaValida (x, y) =
    x >= 1 && y >= 1 && x <= 10 && y <= 10

geraCorpoEmbarcacao :: Coordenada -> Navio -> Orientacao -> [Coordenada]
geraCorpoEmbarcacao (x ,y) navio orient =
    let tam = tamanho navio
    in case orient of
        H -> [ (x+i, y) | i <- [0..tam - 1]]
        V -> [ (x, y+i) | i <- [0..tam - 1]]

-- true: se as posições que vou gerar o navio é valida
-- false: se existir um navio ou a entrada foi inválida
validaPosicionamento :: Tabuleiro -> Coordenada -> Navio -> Orientacao -> Bool
validaPosicionamento tab coord navio orient =
    let corpoNavio = geraCorpoEmbarcacao coord navio orient
        dentroLimite = all coordenadaValida corpoNavio
        celulasLivres = all (\(x, y)-> tab !! (y-1) !! (x-1) == Agua) corpoNavio
    in dentroLimite && celulasLivres

-- Atualiza o tabuleiro para marcar as posições ocupadas com ParteNavio
marcaNaviosNoTabuleiro :: Tabuleiro -> [Coordenada] -> Tabuleiro
marcaNaviosNoTabuleiro tab coords =
  foldl (\t (x,y) -> atualizaCelula t (x-1) (y-1) ParteNavio) tab coords

-- Atualiza uma célula (x,y) no tabuleiro com um valor dado
atualizaCelula :: Tabuleiro -> Int -> Int -> Celula -> Tabuleiro
atualizaCelula tab x y val =
  let (beforeRows, row:afterRows) = splitAt y tab
      (beforeCells, _:afterCells) = splitAt x row
      newRow = beforeCells ++ [val] ++ afterCells
  in beforeRows ++ [newRow] ++ afterRows

-- Posiciona um navio válido no tabuleiro, gerando posições até achar válidas
posicionarNavio :: Tabuleiro -> Navio -> IO (Navio, Tabuleiro)
posicionarNavio tab navio = do
  pos <- geraCoordenada
  orient <- geraOrientacao
  if validaPosicionamento tab pos navio orient
    then do
      let posicoes = geraCorpoEmbarcacao pos navio orient
          novoTab = marcaNaviosNoTabuleiro tab posicoes
          novoNavio = navio { posicoes = posicoes }
      return (novoNavio, novoTab)
    else posicionarNavio tab navio