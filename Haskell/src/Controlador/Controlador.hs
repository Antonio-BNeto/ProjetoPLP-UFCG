{-# LANGUAGE ScopedTypeVariables #-}

module Controlador.Controlador (start) where

import Jogo.Arquitetura
import qualified Logica.Combate as C
import qualified Logica.Posicionamento as P
import qualified Logica.Bot as Bot
import Text.Printf (printf)
-- import System.Console.ANSI (clearScreen, setCursorPosition)


-- Função para ler coordenada do usuário no formato "x y"
lerCoordenada :: IO Coordenada
lerCoordenada = do
  putStrLn "Digite a coordenada para atacar (ex: 3 5):"
  line <- getLine
  let ws = words line
  case ws of
    [sx, sy] -> case (reads sx, reads sy) of
      ([(x,"")], [(y,"")]) -> return (x,y)
      _ -> do
        putStrLn "Entrada inválida. Use dois números separados por espaço."
        lerCoordenada
    _ -> do
      putStrLn "Entrada inválida. Use dois números separados por espaço."
      lerCoordenada

-- Converte uma linha de células para string com ícones
linhaParaStr :: [Celula] -> String
linhaParaStr = unwords . map exibicaoCelula

-- Mostra dois tabuleiros lado a lado com cabeçalhos
mostrarTabuleirosLadoALado :: Tabuleiro -> Tabuleiro -> IO ()
mostrarTabuleirosLadoALado tab1 tab2 = do
  let cabecalho = "    " ++ concatMap (\i -> printf "%2d " (i :: Int)) [0..9]
      separador = replicate 35 '-'

      linhasEsq = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab1
      linhasDir = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab2

      combinadas = zipWith (\l r -> l ++ "     " ++ r) linhasEsq linhasDir

  putStrLn $ "         Seu Tabuleiro" ++ replicate 17 ' ' ++ "Tabuleiro do Inimigo"
  putStrLn $ cabecalho ++ "     " ++ cabecalho
  putStrLn $ separador ++ "     " ++ separador
  mapM_ putStrLn combinadas

-- Loop principal do jogo (turnos)
loopJogo
  :: Tabuleiro       -- Tabuleiro jogador
  -> [Navio]         -- Navios jogador
  -> Tabuleiro       -- Tabuleiro bot
  -> [Navio]         -- Navios bot
  -> [Coordenada]    -- Jogadas já feitas pelo jogador
  -> [Coordenada]    -- Jogadas já feitas pelo bot
  -> IO ()
loopJogo tabJog navJog tabBot navBot tirosJog tirosBot = do
  let ocultar ParteNavio = Agua
      ocultar c = c
      tabBotVisivel = map (map ocultar) tabBot

  -- clearScreen
  -- setCursorPosition 0 0

  putStrLn ""
  mostrarTabuleirosLadoALado tabJog tabBotVisivel

  -- Jogador ataca
  coordJog <- lerCoordenada
  if coordJog `elem` tirosJog
    then do
      putStrLn "Você já atirou nessa coordenada. Tente outra."
      loopJogo tabJog navJog tabBot navBot tirosJog tirosBot
    else do
      let (tabBot', navBot', resJog) = C.realizarAtaque tabBot navBot coordJog
      putStrLn $ "Resultado do seu ataque: " ++ show resJog

      -- Verifica se jogador ganhou
      if C.verificaVitoria navBot'
        then putStrLn "Você venceu! Parabéns!"
        else do
          -- Bot ataca
          coordBot <- Bot.realizarJogadaBot tirosBot
          let (tabJog', navJog', resBot) = C.realizarAtaque tabJog navJog coordBot
          putStrLn $ "Inimigo atacou a coordenada " ++ show coordBot ++ " e o resultado foi: " ++ show resBot

          -- Verifica se bot ganhou
          if C.verificaVitoria navJog'
            then putStrLn "O inimigo venceu! Tente novamente."
            else
              -- Continua o jogo com tabuleiros atualizados e jogadas acumuladas
              loopJogo tabJog' navJog' tabBot' navBot' (coordJog : tirosJog) (coordBot : tirosBot)

-- Função inicial
start :: IO ()
start = do
  putStrLn "Posicionando seus navios..."
  navJog <- Bot.escolherNaviosBot
  let tabJog = foldl (\tab nav -> P.marcaNaviosNoTabuleiro tab (posicoes nav)) criacaoTabuleiro navJog

  putStrLn "Posicionando navios inimigos..."
  navBot <- Bot.escolherNaviosBot
  let tabBot = foldl (\tab nav -> P.marcaNaviosNoTabuleiro tab (posicoes nav)) criacaoTabuleiro navBot

  putStrLn "Começando o jogo!"
  loopJogo tabJog navJog tabBot navBot [] []






























































-- module Controlador.Controlador (start) where

-- import Jogo.Arquitetura
-- import qualified Logica.Combate as C
-- import qualified Logica.Posicionamento as P
-- import qualified Logica.Bot as Bot
-- import Text.Printf (printf)

-- -- Função para ler coordenada do usuário no formato "x y"
-- lerCoordenada :: IO Coordenada
-- lerCoordenada = do
--   putStrLn "Digite a coordenada para atacar (ex: 3 5):"
--   line <- getLine
--   let ws = words line
--   case ws of
--     [sx, sy] -> case (reads sx, reads sy) of
--       ([(x,"")], [(y,"")]) -> return (x,y)
--       _ -> do
--         putStrLn "Entrada inválida. Use dois números separados por espaço."
--         lerCoordenada
--     _ -> do
--       putStrLn "Entrada inválida. Use dois números separados por espaço."
--       lerCoordenada

-- -- Converte uma linha de células para string com ícones
-- linhaParaStr :: [Celula] -> String
-- linhaParaStr = unwords . map exibicaoCelula

-- --Mostra dois tabuleiros lado a lado com cabeçalhos
-- mostrarTabuleirosLadoALado :: Tabuleiro -> Tabuleiro -> IO ()
-- mostrarTabuleirosLadoALado tab1 tab2 = do
--   let cabecalho = "    " ++ concatMap (\i -> printf "%2d " i) [0..9]
--       separador = replicate 33 '-'

--       linhasEsq = zipWith (\i linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab1
--       linhasDir = zipWith (\i linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab2

--       combinadas = zipWith (\l r -> l ++ "     " ++ r) linhasEsq linhasDir

--   putStrLn $ "   Seu Tabuleiro" ++ replicate 14 ' ' ++ "Tabuleiro do Inimigo"
--   putStrLn $ cabecalho ++ "     " ++ cabecalho
--   putStrLn $ separador ++ "     " ++ separador
--   mapM_ putStrLn combinadas


-- -- Mostra o tabuleiro na tela (para o jogador)
-- -- Loop principal do jogo (turnos)
-- loopJogo
--   :: Tabuleiro       -- Tabuleiro jogador
--   -> [Navio]         -- Navios jogador
--   -> Tabuleiro       -- Tabuleiro bot
--   -> [Navio]         -- Navios bot
--   -> [Coordenada]    -- Jogadas já feitas pelo jogador
--   -> [Coordenada]    -- Jogadas já feitas pelo bot
--   -> IO ()
-- loopJogo tabJog navJog tabBot navBot tirosJog tirosBot = do
--   let ocultar ParteNavio = Agua
--       ocultar c = c
--       tabBotVisivel = map (map ocultar) tabBot

--   putStrLn ""
--   mostrarTabuleirosLadoALado tabJog tabBotVisivel

--   -- Jogador ataca
--   coordJog <- lerCoordenada
--   if coordJog `elem` tirosJog
--     then do
--       putStrLn "Você já atirou nessa coordenada. Tente outra."
--       loopJogo tabJog navJog tabBot navBot tirosJog tirosBot
--     else do
--       let (tabBot', navBot', resJog) = C.realizarAtaque tabBot navBot coordJog
--       putStrLn $ "Resultado do seu ataque: " ++ show resJog

--       -- Verifica se jogador ganhou
--       if C.verificaVitoria navBot'
--         then putStrLn "Você venceu! Parabéns!"
--         else do
--           -- Bot ataca
--           coordBot <- Bot.realizarJogadaBot tirosBot
--           let (tabJog', navJog', resBot) = C.realizarAtaque tabJog navJog coordBot
--           putStrLn $ "Inimigo atacou a coordenada " ++ show coordBot ++ " e o resultado foi: " ++ show resBot

--           -- Verifica se bot ganhou
--           if C.verificaVitoria navJog'
--             then putStrLn "O inimigo venceu! Tente novamente."
--             else
--               -- Continua o jogo com tabuleiros atualizados e jogadas acumuladas
--               loopJogo tabJog' navJog' tabBot' navBot' (coordJog : tirosJog) (coordBot : tirosBot)

-- -- Função inicial
-- start :: IO ()
-- start = do
--   putStrLn "Posicionando seus navios..."
--   navJog <- Bot.escolherNaviosBot
--   let tabJog = foldl (\tab nav -> P.marcaNaviosNoTabuleiro tab (posicoes nav)) criacaoTabuleiro navJog

--   putStrLn "Posicionando navios inimigos..."
--   navBot <- Bot.escolherNaviosBot
--   let tabBot = foldl (\tab nav -> P.marcaNaviosNoTabuleiro tab (posicoes nav)) criacaoTabuleiro navBot

--   putStrLn "Começando o jogo!"
--   loopJogo tabJog navJog tabBot navBot [] []