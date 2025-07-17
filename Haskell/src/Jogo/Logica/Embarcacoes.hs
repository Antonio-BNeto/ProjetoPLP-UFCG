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

-- tipo que representa uma embacação no jogo
data Embarcacao = Embarcacao
    { tipo :: TipoEmbarcacao
    , posicoes :: [Posicao]
    , atingida :: [Bool]
    } deriving (Show, Eq)
