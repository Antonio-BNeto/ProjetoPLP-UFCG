module Util.IOUtils where

import Jogo.Arquitetura (Orientacao)
lerOrientacao :: IO Orientacao
lerOrientacao = do
    putStr "Orientação (h para horizontal, v para vertical)"
    input <- getLine
    case map toLower input of
        "h" -> return H
        "v" -> return V
        _  -> do
            putStr "Erro: Opção inválida! Use 'h' ou 'v' ."
            lerOrientacao
