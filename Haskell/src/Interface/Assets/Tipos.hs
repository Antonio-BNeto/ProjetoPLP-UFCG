module Tipos (
    Assets(..),
    ModoDeJogo(..),
    Jogador(..),
    EstadoDoJogo(..),
    EstadoDeConfiguracao(..),
    EstadoDaAplicacao(..)
) where

import Jogo.Arquitetura

-- Tipo Assets simplificado, contendo apenas as artes que estão em uso.
data Assets = Assets {
    arteMenu          :: String,
    arteVitoria       :: String,
    arteDerrota       :: String,
    arteTelaTransicao :: String
}

-- Define os modos de jogo possíveis.
data ModoDeJogo = SinglePlayer | MultiPlayer deriving (Eq, Show)

-- Define os jogadores.
data Jogador = Jogador1 | Jogador2 deriving (Show, Eq)

-- Guarda todas as informações de uma partida em andamento
data EstadoDoJogo = EstadoDoJogo {
      tabuleiroP1 :: Tabuleiro,
      naviosP1    :: [Navio],
      tabuleiroP2 :: Tabuleiro,
      naviosP2    :: [Navio],
      turno       :: Jogador,
      modo        :: ModoDeJogo
}

-- Guarda as informações necessárias durante a fase de configuração
data EstadoDeConfiguracao = EstadoDeConfiguracao {
      modoConfig      :: ModoDeJogo,
      jogadorConfig   :: Jogador,
      tabuleiroConfig :: Tabuleiro,
      frotaPendente   :: [Navio]
}

-- O estado principal da aplicação (a "tela" atual)
data EstadoDaAplicacao
    = Menu
    | ConfigurandoJogo EstadoDeConfiguracao
    | EmJogo EstadoDoJogo
    | TransicaoDeTurno EstadoDoJogo
    | FimDeJogo EstadoDoJogo
    | Saindo