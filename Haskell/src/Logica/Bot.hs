module Logica.Bot
  ( escolherNaviosBot
  , realizarJogadaBot
  ) where

import Jogo.Arquitetura
import System.Random.Stateful (uniformRM, globalStdGen)

-- Usa as orientações que a lógica de posicionamento espera
data Orientacao = H | V deriving (Eq, Show)

-- Navios padrão com tipo e tamanho, sem posições e partes atingidas inicialmente
naviosPadrao :: [Navio]
naviosPadrao =
  [ Navio "Porta-Avioes" 5 [] []
  , Navio "Encouracado" 4 [] []
  , Navio "Submarino" 3 [] []
  , Navio "Cruzador"   3 [] []
  , Navio "Destroyer"  2 [] []
  ]

-- Gera coordenada aleatória entre 1 e tamanhoTabuleiro
geraCoordenada :: IO Coordenada
geraCoordenada = do
  x <- uniformRM (1 :: Int, tamanhoTabuleiro :: Int) globalStdGen
  y <- uniformRM (1 :: Int, tamanhoTabuleiro :: Int) globalStdGen
  return (x, y)

-- Gera orientação aleatória H ou V
geraOrientacao :: IO Orientacao
geraOrientacao = do
  n <- uniformRM (0 :: Int, 1 :: Int) globalStdGen
  return $ if n == 0 then H else V

-- Gera as posições do navio (mesma lógica do posicionamento)
geraCorpoEmbarcacao :: Coordenada -> Navio -> Orientacao -> [Coordenada]
geraCorpoEmbarcacao (x,y) navio orient =
  let tam = tamanho navio
  in case orient of
    H -> [ (x+i, y) | i <- [0..tam-1] ]
    V -> [ (x, y+i) | i <- [0..tam-1] ]

-- VALIDA se o posicionamento é válido: dentro dos limites e sem colisão com ParteNavio
validaPosicionamento :: Tabuleiro -> Coordenada -> Navio -> Orientacao -> Bool
validaPosicionamento tab (x,y) navio orient =
  let posicoes = geraCorpoEmbarcacao (x,y) navio orient
      dentroDoTabuleiro (a,b) = a >= 1 && a <= tamanhoTabuleiro && b >= 1 && b <= tamanhoTabuleiro
      celulaLivre (a,b) = (tab !! (a-1) !! (b-1)) == Agua
  in all dentroDoTabuleiro posicoes && all celulaLivre posicoes

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
  let todas = [(x,y) | x <- [1..tamanhoTabuleiro], y <- [1..tamanhoTabuleiro]]
      restantes = filter (`notElem` tirosDados) todas
  if null restantes
    then error "Sem posições restantes para jogar"
    else do
      idx <- uniformRM (0, length restantes - 1) globalStdGen
      return (restantes !! idx)
