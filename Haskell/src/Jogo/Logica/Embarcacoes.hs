module Jogo.Logica.Embarcacoes where

-- Definindo um tipo de dado que possui 3 valores possiveis, sendo eles  (Navio, PortaAviao, Submarino)
data TipoEmbarcacao = PortaAvioes    -- tamanho 5 [P]
                    | Cruzador       -- tamanho 4 [C]
                    | ContraTorpedo  -- tamanho 3 [T]
                    | Submarino      -- tamanho 3 [S]
                    | Barco          -- tamanho 2 [B]
                    deriving (Eq, Show, Enum, Bounded)

-- (ideia: quantidade de Embarcacoes){
--    PortaAvioes:      1 x 5 = 5 espaços
--    Cruzador:         2 x 4 = 8 espaços 
--    ContraTorpedos:   3 x 3 = 9 espaços
--    Submarino:        3 x 3 = 9 espaços
--    Barco:            4 x 2 = 8 espaços
-- } 
-- ocupa no total 39 espaços do tabuleiro

tamanhoEmbarcacao :: TipoEmbarcacao -> Int
tamanhoEmbarcacao PortaAvioes   = 4
tamanhoEmbarcacao Cruzador      = 4
tamanhoEmbarcacao ContraTorpedo = 3
tamanhoEmbarcacao Submarino     = 3
tamanhoEmbarcacao Barco         = 2

representacaoVisual :: TipoEmbarcacao -> String
representacaoVisual PortaAvioes   = "P"
representacaoVisual Cruzador      = "C"
representacaoVisual ContraTorpedo = "T"
representacaoVisual Submarino     = "S"
representacaoVisual Barco         = "B"

-- Tipo que vai representar uma posição no tabuleiro (linha, coluna)
data Posicao = Posicao Int Int deriving (Eq, Show)

-- Tipo que vai representar a orientação que a embarcação será colocada
data Orientacao = Horizontal | Vertical deriving (Eq, Show)

-- Função que com base no char que o usuário passar vai dizer qual é a orientação
charParaOrientacao :: Char -> Maybe Orientacao
charParaOrientacao 'h' = Just Horizontal
charParaOrientacao 'v' = Just Vertical
charParaOrientacao 'H' = Just Horizontal
charParaOrientacao 'V' = Just Vertical
charParaOrientacao _   = Nothing


-- Posicao (x, y) -> linha coluna
-- TipoEmbacacao (qual é a embarcacao para assim definir o tamanho)
-- Char (v -> Vertical) ou (h -> Horizontal)
-- [Posicao] : Um array com as posições que a embarcação vai ocupar
geraCorpoEmbarcacao :: Posicao -> TipoEmbarcacao -> Orientacao -> [Posicao]
geraCorpoEmbarcacao (Posicao x y) tipo orient =
    let tamanho = tamanhoEmbarcacao tipo
    in case orient of
        Horizontal -> [Posicao x (y + i) | i <- [0..tamanho - 1]]
        Vertical   -> [Posicao (x + i) y | i <- [0..tamanho - 1]]

-- Tipo que representa uma embarcação no jogo
data Embarcacao = Embarcacao
    { tipo :: TipoEmbarcacao
    , posicoes :: [Posicao]
    , atingida :: [Bool]
    } deriving (Show, Eq)

-- Função que cria uma nova embarcação completa
criaEmbarcacaoCompleta :: Posicao -> TipoEmbarcacao -> Orientacao -> Embarcacao
criaEmbarcacaoCompleta cabeca tipo orient =
    let posicoes = geraCorpoEmbarcacao cabeca tipo orient
    in Embarcacao
        { tipo = tipo
        , posicoes = posicoes
        , atingida = replicate (length posicoes) False
        }


-- Função de validação para quando gerar o corpo ele não sair do tabuleiro


-- Função para saber se o corpo gerado
-- não está sobrepondo alguma outra embarcação (responsabilidade do Embarcacoes.hs?)