{-# LANGUAGE ScopedTypeVariables #-}

module Controlador.Controlador (start) where

import Jogo.Arquitetura
import qualified Logica.Combate as Combate
import qualified Logica.Posicionamento as Posic
import qualified Logica.Bot as Bot
import Interface.Arte
import System.IO (hFlush, stdout)
import System.Console.ANSI (clearScreen)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)


-- CONSTANTES
delayMenu :: Int
delayMenu = 1500000  -- 1.5 segundos

delayJogo :: Int
delayJogo = 1000000   -- 1 segundo

-- MAIN LOOP
start :: IO ()
start = do
    clearScreen
    showMainMenu

-- MENU PRINCIPAL
showMainMenu :: IO ()
showMainMenu = do
    displayArt Menu
    putStrLn "\nMENU PRINCIPAL:"
    putStrLn "1. Novo Jogo"
    putStrLn "2. Sair"
    putStr "Selecione uma opção: "
    hFlush stdout
    
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Iniciando novo jogo..."
            threadDelay delayMenu
            startGame
        "2" -> do
            displayArt Goodbye
            putStrLn "Obrigado por jogar!"
            threadDelay delayMenu
            return ()
        _ -> do
            putStrLn "Opção inválida! Tente novamente."
            threadDelay delayMenu
            showMainMenu

-- INICIALIZAÇÃO DO JOGO
startGame :: IO ()
startGame = do
    clearScreen
    putStrLn "Preparando o campo de batalha..."
    threadDelay delayJogo
    
    -- Posicionamento dos navios
    putStrLn "\nPosicionando seus navios..."
    navJog <- Bot.escolherNaviosBot
    let tabJog = foldl (\tab nav -> Posic.marcaNaviosNoTabuleiro tab (posicoes nav)) 
                       criacaoTabuleiro navJog
    
    threadDelay delayJogo

    putStrLn "Posicionando navios inimigos..."
    navBot <- Bot.escolherNaviosBot
    let tabBot = foldl (\tab nav -> Posic.marcaNaviosNoTabuleiro tab (posicoes nav)) 
                       criacaoTabuleiro navBot
    
    threadDelay delayJogo

    putStrLn "\nTudo pronto! A batalha vai começar!"
    threadDelay delayJogo
    
    loopJogo tabJog navJog tabBot navBot [] [] []

-- LOOP PRINCIPAL DO JOGO
loopJogo :: Tabuleiro -> [Navio] -> Tabuleiro -> [Navio] 
         -> [Coordenada] -> [Coordenada] -> [Combate.ResultadoAtaque] -> IO ()
loopJogo tabJog navJog tabBot navBot tirosJog tirosBot resultadosBot = do
    let ocultarNavios ParteNavio = Agua
        ocultarNavios celula = celula
        tabBotVisivel = map (map ocultarNavios) tabBot
    
    clearScreen
    mostrarTabuleirosLadoALado tabJog tabBotVisivel
    
    -- Turno do Jogador
    putStrLn "\nSEU TURNO:"
    coordJog <- lerCoordenada
    
    if coordJog `elem` tirosJog
        then do
            putStrLn "Você já atirou nesta posição!"
            threadDelay delayJogo
            loopJogo tabJog navJog tabBot navBot tirosJog tirosBot resultadosBot
        else do
            let (tabBot', navBot', resJog) = Combate.realizarAtaque tabBot navBot coordJog
            
            -- Feedback visual do resultado
            clearScreen
            mostrarTabuleirosLadoALado tabJog tabBotVisivel
            mostrarResultado resJog "Você"
            threadDelay delayJogo
            
            -- Verificar vitória
            if Combate.verificaVitoria navBot'
                then fimJogo Vitoria "Vitória! Todos os navios inimigos foram afundados!"
                else turnoBot tabJog navJog tabBot' navBot' tirosJog coordJog tirosBot resultadosBot

-- TURNO DO BOT
turnoBot :: Tabuleiro -> [Navio] -> Tabuleiro -> [Navio]
         -> [Coordenada] -> Coordenada -> [Coordenada] -> [Combate.ResultadoAtaque]
         -> IO ()
turnoBot tabJog navJog tabBot navBot tirosJog coordJog tirosBot resultadosBot = do
    clearScreen
    let ocultarNavios ParteNavio = Agua
        ocultarNavios celula = celula
    mostrarTabuleirosLadoALado tabJog (map (map ocultarNavios) tabBot)
    putStrLn "\nTURNO DO INIMIGO..."
    threadDelay delayJogo
    
    coordBot <- Bot.realizarJogadaBot tirosBot resultadosBot
    let (tabJog', navJog', resBot) = Combate.realizarAtaque tabJog navJog coordBot
    
    -- Feedback do ataque do bot
    clearScreen
    mostrarTabuleirosLadoALado tabJog' (map (map ocultarNavios) tabBot)
    putStrLn $ "\nO inimigo atacou: " ++ show coordBot
    mostrarResultado resBot "O inimigo"
    threadDelay delayJogo
    
    -- Verificar derrota
    if Combate.verificaVitoria navJog'
        then fimJogo Derrota "Derrota! Seus navios foram afundados."
        else loopJogo tabJog' navJog' tabBot navBot 
                      (coordJog:tirosJog) (coordBot:tirosBot) (resBot:resultadosBot)

-- FIM DE JOGO
fimJogo :: ArtType -> String -> IO ()
fimJogo arte mensagem = do
    showScreen arte mensagem
    threadDelay (2 * delayJogo)
    showMainMenu

-- FUNÇÕES AUXILIARES

-- Mostra resultado de um ataque
mostrarResultado :: Combate.ResultadoAtaque -> String -> IO ()
mostrarResultado resultado jogador = case resultado of
    Combate.Afundou navio -> 
        putStrLn $ "\n" ++ jogador ++ " afundou o " ++ tipo navio ++ "!"
    Combate.Acertou navio -> 
        putStrLn $ "\n" ++ jogador ++ " acertou o " ++ tipo navio ++ "!"
    Combate.TiroFora -> 
        putStrLn $ "\n" ++ jogador ++ " errou o tiro!"
    Combate.AcertoRepetido -> 
        putStrLn $ "\n" ++ jogador ++ " atirou em um local já atingido!"
    Combate.ErroRepetido -> 
        putStrLn $ "\n" ++ jogador ++ " atirou na água novamente!"
    _ -> return ()

-- Ler coordenada do jogador
lerCoordenada :: IO Coordenada
lerCoordenada = do
    putStr "Digite as coordenadas (linha coluna): "
    hFlush stdout
    input <- getLine
    case words input of
        [xStr, yStr] -> case (reads xStr, reads yStr) of
            ([(x,"")], [(y,"")]) | coordenadaValida (x,y) -> return (x,y)
            _ -> do
                putStrLn "Coordenadas inválidas. Use números entre 0 e 9."
                lerCoordenada
        _ -> do
            putStrLn "Formato inválido. Use: linha coluna (ex: 3 5)"
            lerCoordenada

-- Verifica se coordenada está dentro do tabuleiro
coordenadaValida :: Coordenada -> Bool
coordenadaValida (x,y) = x >= 0 && y >= 0 && x < tamanhoTabuleiro && y < tamanhoTabuleiro

-- Exibe uma linha do tabuleiro
linhaParaStr :: [Celula] -> String
linhaParaStr = unwords . map exibicaoCelula

-- Mostra tabuleiros lado a lado
mostrarTabuleirosLadoALado :: Tabuleiro -> Tabuleiro -> IO ()
mostrarTabuleirosLadoALado tab1 tab2 = do
  displayArt TelaJogo
  let cabecalho = "    " ++ concatMap (\i -> printf "%2d " (i :: Int)) [0..9]
      separador = replicate 35 '-'
      linhasEsq = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab1
      linhasDir = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab2
      combinadas = zipWith (\l r -> l ++ "     " ++ r) linhasEsq linhasDir

  putStrLn $ "\n            Seu Tabuleiro" ++ replicate 17 ' ' ++ "      Tabuleiro do Inimigo"
  putStrLn $ cabecalho ++ "     " ++ cabecalho
  putStrLn $ separador ++ "     " ++ separador
  mapM_ putStrLn combinadas
  putStrLn ""

-- Função para mostrar tela com arte e mensagem
showScreen :: ArtType -> String -> IO ()
showScreen artType msg = do
    displayArt artType
    putStrLn ""
    putStrLn msg