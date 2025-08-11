{-# LANGUAGE ScopedTypeVariables #-}

module Controlador.Controlador where

import Jogo.Arquitetura
import Interface.Arte
import Logica.Bot (escolherNaviosBot, realizarJogadaBot)
import Logica.Combate (realizarAtaque, verificaVitoria, ResultadoAtaque(..))
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.Console.ANSI (clearScreen)

-- Constantes de tempo para pausas
delayMenu :: Int
delayMenu = 1500000

delayLoading :: Int
delayLoading = 500000

delayGame :: Int
delayGame = 1000000 

-- Ponto de entrada da aplicação
start :: IO ()
start = do
  clearScreen
  mostrarTelaInicial

-- Exibe o menu principal
mostrarTelaInicial :: IO ()
mostrarTelaInicial = do
    clearScreen
    displayArt Menu
    putStrLn "\n                                      Escolha uma opção:"
    putStrLn "                                      [1] Novo Jogo"
    putStrLn "                                      [2] Sair"
    putStrLn "                                      [3] Como Funciona"
    putStr "                                      > "
    hFlush stdout
    
    option <- getLine
    case option of
        "1" -> startGame
        "2" -> displayArt Adeus >> threadDelay delayMenu
        "3" -> mostrarDescricao
        _   -> putStrLn "Opção inválida!" >> threadDelay delayMenu >> mostrarTelaInicial

-- Explicação completa do funcionamento do Jogo
mostrarDescricao :: IO ()
mostrarDescricao = do
    clearScreen
    displayArt Jogo
    putStrLn "COMO FUNCIONA:"
    putStrLn ""
    putStrLn "Bem-vindo ao clássico jogo de Batalha Naval!"
    putStrLn "Sua missão: derrotar o inimigo afundando toda a frota dele."
    putStrLn ""
    putStrLn "OBJETIVO:"
    putStrLn "  ⚓ Afundar todas as embarcações inimigas antes que a sua frota seja destruída."
    putStrLn ""
    putStrLn "POSICIONAMENTO DOS NAVIOS:"
    putStrLn "  - As embarcações são posicionadas AUTOMATICAMENTE."
    putStrLn "  - Você não precisa se preocupar em posicioná-las manualmente."
    putStrLn ""
    putStrLn "FROTA DISPONÍVEL:"
    putStrLn  "- Porta-Avioes (5 espaços)"
    putStrLn  "- Encouracado (4 espaços)"
    putStrLn  "- Submarino (3 espaços)"
    putStrLn  "- Cruzador (3 espaços)"
    putStrLn  "- Destroyer (2 espaços)"
    putStrLn ""
    putStrLn "COMO ATACAR:"
    putStrLn "  - O tabuleiro possui linhas e colunas numeradas."
    putStrLn "  - Para atacar, digite as coordenadas no formato:"
    putStrLn "        linha coluna"
    putStrLn "    (Exemplo: '3 5' -> significa linha 3, coluna 5.)"
    putStrLn ""
    putStrLn "FEEDBACK DO ATAQUE:"
    putStrLn "  🔥  : Acerto em uma embarcação"
    putStrLn "  ❌  : Tiro na água"
    putStrLn "  🚢  : Parte de navio"
    putStrLn "  🌊  : Água"
    putStrLn ""
    putStrLn "DICA DE ESTRATÉGIA:"
    putStrLn "  Use a lógica: após um acerto, ataque nas casas vizinhas para"
    putStrLn "  aumentar as chances de afundar o navio."
    putStrLn ""
    putStrLn "Pressione Enter para voltar ao menu principal..."
    _ <- getLine
    mostrarTelaInicial


-- Prepara e inicia uma nova partida
startGame :: IO ()
startGame = do
    clearScreen

    putStrLn "Posicionando seus navios..."
    (navJog, tabJog) <- escolherNaviosBot
    threadDelay delayLoading

    putStrLn "Posicionando os navios do bot..."
    (navBot, tabBot) <- escolherNaviosBot
    threadDelay delayLoading

    putStrLn "Preparando campo de batalha..."
    threadDelay delayLoading

    putStrLn "Navios posicionados. Que a batalha comece!"
    putStrLn "\nPressione Enter para iniciar..."
    _ <- getLine

    loopJogo tabJog navJog tabBot navBot [] []

-- Loop principal do jogo
loopJogo :: Tabuleiro -> [Navio] -> Tabuleiro -> [Navio] -> [Coordenada] -> [Coordenada] -> IO ()
loopJogo tabJog navJog tabBot navBot tirosJog tirosBot = do
    clearScreen
    let tabBotVisivel = map (map ocultar) tabBot

    displayArt Jogo
    mostrarTabuleirosLadoALado tabJog tabBotVisivel

    -- Vez do jogador
    putStrLn "\nSua vez de atirar!"
    coordJog <- lerCoordenada
    let (tabBotAtualizado, navBotAtualizado, res) = realizarAtaque tabBot navBot coordJog
    
    clearScreen
    displayArt Jogo
    mostrarTabuleirosLadoALado tabJog (map (map ocultar) tabBotAtualizado)
    mostrarResultado "Você" res

    -- Verifica o resultado do tiro para decidir o próximo passo
    case res of
      -- Se a jogada foi inválida (repetida), o jogador joga de novo.
      AcertoRepetido -> jogadaInvalida
      ErroRepetido   -> jogadaInvalida
      
      -- Se a jogada foi válida, continua o fluxo normal.
      _ -> do
        -- Verifica vitoria do jogador
        if verificaVitoria navBotAtualizado
          then do
            fimDeJogo Vitoria "Parabéns!  afundou todos os navios inimigos e venceu!"
          else do
            -- Passa a vez para o bot
            turnoDoBot tabJog navJog tabBotAtualizado navBotAtualizado (coordJog:tirosJog) tirosBot
  where
    -- Ação para jogada inválida: pausa breve e joga novamente.
    jogadaInvalida = do
      threadDelay delayGame -- Pausa 
      loopJogo tabJog navJog tabBot navBot tirosJog tirosBot -- Recomeça o turno do jogador.

-- Função para o turno do Bot
turnoDoBot :: Tabuleiro -> [Navio] -> Tabuleiro -> [Navio] -> [Coordenada] -> [Coordenada] -> IO ()
turnoDoBot tabJog navJog tabBot navBot tirosJog tirosBot = do
    threadDelay delayLoading
    putStrLn "\nTurno do inimigo."
    putStrLn "Pressione Enter para continuar..."
    _ <- getLine
    
    coordBot <- realizarJogadaBot tirosBot
    let (tabJogAtualizado, navJogAtualizado, resBot) = realizarAtaque tabJog navJog coordBot
    
    clearScreen
    displayArt Jogo
    mostrarTabuleirosLadoALado tabJogAtualizado (map (map ocultar) tabBot)
    putStrLn $ "\nO inimigo atacou em: " ++ show coordBot
    threadDelay delayLoading
    mostrarResultado "O inimigo" resBot
    threadDelay delayLoading

    -- Verifica vitoria do bot
    if verificaVitoria navJogAtualizado
      then do
        fimDeJogo Derrota "O inimigo afundou todos os seus navios! Você perdeu."
      else do
        putStrLn "\nPressione Enter para continuar..."
        _ <- getLine
        loopJogo tabJogAtualizado navJogAtualizado tabBot navBot tirosJog (coordBot:tirosBot)

-- Função para finalizar o jogo
fimDeJogo :: ArtType -> String -> IO ()
fimDeJogo arte mensagem = do
    clearScreen
    displayArt arte
    putStrLn $ "\n" ++ mensagem
    putStrLn "\nPressione Enter para voltar ao menu..."
    _ <- getLine
    mostrarTelaInicial

-- FUNÇÕES AUXILIARES

-- Oculta a posição dos navios inimigos não atingidos
ocultar :: Celula -> Celula
ocultar ParteNavio = Agua
ocultar c = c

-- Mostra dois tabuleiros lado a lado com cabeçalhos centralizados
mostrarTabuleirosLadoALado :: Tabuleiro -> Tabuleiro -> IO ()
mostrarTabuleirosLadoALado tab1 tab2 = do
    let center width str =
            let len = length str
                padding = width - len
                leftPad = padding `div` 2
                rightPad = padding - leftPad
            in replicate leftPad ' ' ++ str ++ replicate rightPad ' '

    let boardWidth = 34
        gap = "     "
    let title1 = "Seu Tabuleiro"
        title2 = "Tabuleiro do Inimigo"
        titleLine = center boardWidth title1 ++ gap ++ center boardWidth title2

    let cabecalho = "    " ++ concatMap (\i -> printf "%2d " (i :: Int)) [0..tamanhoTabuleiro-1]
        separador = replicate boardWidth '-'
    let linhasEsq = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab1
        linhasDir = zipWith (\(i :: Int) linha -> printf "%2d | %s" i (linhaParaStr linha)) [0..] tab2
    let combinadas = zipWith (\l r -> l ++ gap ++ r) linhasEsq linhasDir

    putStrLn $ "\n" ++ titleLine
    putStrLn $ cabecalho ++ gap ++ cabecalho
    putStrLn $ separador ++ gap ++ separador
    mapM_ putStrLn combinadas

-- Mostra resultados de forma mais amigável
mostrarResultado :: String -> ResultadoAtaque -> IO ()
mostrarResultado jogador resultado =
    let msg = case resultado of
            Acertou navio -> "acertou o navio " ++ navio ++ "!"
            Afundou navio -> "AFUNDOU o navio " ++ navio ++ "!"
            TiroFora      -> "errou."
            _             -> "já atirou nesta posição."
    in putStrLn $ "\n" ++ jogador ++ " " ++ msg

-- Lê coordenadas do usuário
lerCoordenada :: IO Coordenada
lerCoordenada = do
    putStr "Digite a coordenada para atacar (ex: 3 5): "
    hFlush stdout
    line <- getLine
    case words line of
      [sx, sy] -> case (reads sx, reads sy) of
        ([(x,"")], [(y,"")]) ->
          if x >= 0 && x < tamanhoTabuleiro && y >= 0 && y < tamanhoTabuleiro
          then return (x, y)
          else do
            putStrLn "Coordenada fora do tabuleiro. Tente novamente."
            lerCoordenada
        _ -> do
          putStrLn "Entrada inválida. Use dois números separados por espaço."
          lerCoordenada
      _ -> do
        putStrLn "Formato inválido. Tente novamente."
        lerCoordenada

-- Converte uma linha de células para string com ícones
linhaParaStr :: [Celula] -> String
linhaParaStr = unwords . map exibicaoCelula
