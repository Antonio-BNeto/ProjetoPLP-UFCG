{-# LANGUAGE BlockArguments #-}
module Logica.Combate (
    ResultadoAtaque(..),
    realizarAtaque,
    verificaVitoria,
    verificaDerrota
) where

import Jogo.Arquitetura
import qualified Jogo.Tabuleiro as Tabuleiro
import qualified Jogo.Navio as Navio

data ResultadoAtaque
    = TiroFora
    | AcertoRepetido
    | ErroRepetido
    | Acertou Navio
    | Afundou Navio
    | CoordenadaInvalida
    deriving (Eq, Show)

-- | Realiza um ataque no tabuleiro e retorna o novo estado do jogo
realizarAtaque :: Tabuleiro -> [Navio] -> Coordenada -> (Tabuleiro, [Navio], ResultadoAtaque)
realizarAtaque tabuleiro navios coord@(x,y)
    | x < 0 || x >= tamanhoTabuleiro || y < 0 || y >= tamanhoTabuleiro =
        (tabuleiro, navios, CoordenadaInvalida)
    | otherwise =
        case Tabuleiro.obter coord tabuleiro of
            Nothing -> (tabuleiro, navios, CoordenadaInvalida)
            Just Agua ->
                let novoTab = Tabuleiro.marca coord Erro tabuleiro
                in (novoTab, navios, TiroFora)
            Just ParteNavio ->
                let novoTab = Tabuleiro.marca coord Atingido tabuleiro
                    naviosAtualizados = Navio.atualizaNavios coord navios
                in case Navio.encontraNavio coord naviosAtualizados of
                    Just navio ->
                        if Navio.navioAfundado navio
                            then (novoTab, naviosAtualizados, Afundou navio)
                            else (novoTab, naviosAtualizados, Acertou navio)
                    Nothing -> (novoTab, naviosAtualizados, AcertoRepetido)
            Just Atingido -> (tabuleiro, navios, AcertoRepetido)
            Just Erro -> (tabuleiro, navios, ErroRepetido)

-- | Verifica se todos os navios foram afundados (condição de vitória)
verificaVitoria :: [Navio] -> Bool
verificaVitoria navios = not (null navios) && all Navio.navioAfundado navios

-- | Verifica se todos os navios foram afundados (condição de derrota)
verificaDerrota :: [Navio] -> Bool
verificaDerrota = verificaVitoria