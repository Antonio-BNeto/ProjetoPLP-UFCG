module Util.Lista (atualizaIndice) where

atualizaIndice :: Int -> a -> [a] -> [a]
atualizaIndice 0 novo (_:xs) = novo : xs
atualizaIndice n novo (x:xs) = x : atualizaIndice (n-1) novo xs
atualizaIndice _ _   []      = []