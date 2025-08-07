module Util.Lista (atualizaIndice) where

atualizaIndice :: Int -> a -> [a] -> [a]
atualizaIndice 0 novoValor (_:xs) = novoValor : xs
atualizaIndice n novoValor (x:xs) = x : atualizaIndice (n-1) novoValor xs
atualizaIndice _ _   []      = []