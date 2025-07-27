module Jogo.Arquitetura where

-- Determinando um tipo "Coordenada"
type Coordenada = (Int, Int)

-- Estrutura do navio
data Navio = Navio {
    tipo :: String,
    tamanho :: Int,
    posicoes :: [Coordenada],
    partesAtingidas :: [Coordenada] 
} deriving (Show, Eq)

-- Definindo os "estados" de cada célula
data Celula = Agua 
            | ParteNavio 
            | Atingido 
            | Erro
            deriving (Eq)

-- Exibição da célula
exibicaoCelula :: Celula -> String
exibicaoCelula Agua = "🌊"
exibicaoCelula ParteNavio = "🚢"
exibicaoCelula Atingido = "✅"
exibicaoCelula Erro = "❌"

-- Definindo o tamanho padrão do tabuleiro
tamanhoTabuleiro :: Int
tamanhoTabuleiro = 10

-- Determinando o tipo "Tabuleiro" como uma matriz de "Celula"
type Tabuleiro = [[Celula]]

-- Cria um Tabuleiro só com água (vazio)
criacaoTabuleiro :: Tabuleiro
criacaoTabuleiro = replicate tamanhoTabuleiro (replicate tamanhoTabuleiro Agua)