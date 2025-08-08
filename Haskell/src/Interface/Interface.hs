module Interface (
    limparTela,
    carregarAssets,
    desenhar
) where

import Tipos
import System.Console.ANSI
import System.Process (system)

limparTela :: IO ()
limparTela = do
    _ <- system "cls"
    _ <- system "clear"
    return ()

carregarAssets :: IO Assets
carregarAssets = do
    putStrLn "Carregando assets..."
    menu            <- readFile "Assets/menu_art.txt"
    vitoria         <- readFile "Assets/vitoria_art.txt"
    derrota         <- readFile "Assets/derrota_art.txt"
    telaTransicao   <- readFile "Assets/telaTransicao_art.txt"

    return $ Assets {
        arteMenu = menu,
        arteVitoria = vitoria,
        arteDerrota = derrota,
        arteTelaTransicao = telaTransicao
    }

desenhar :: Assets -> EstadoDaAplicacao -> IO ()

desenhar assets Menu = do
    limparTela
    putStrLn (arteMenu assets)
    putStrLn ""
    putStrLn "1. 1 Jogador"
    putStrLn "2. 2 Jogadores"
    putStrLn "3. Sair"

desenhar assets (ConfigurandoJogo config) = do
    limparTela
    putStrLn "=================================================="
    putStrLn $ "      Montagem do Tabuleiro: " ++ show (jogadorConfig config)
    putStrLn "=================================================="
    putStrLn ""

    desenharTabuleiro (tabuleiroConfig config)
    putStrLn ""

    if null (frotaPendente config) then
        putStrLn "Todos os navios foram posicionados! Pressione [Enter]."
    else do
        let proximoNavio = head (frotaPendente config)
        putStrLn $ "Posicione seu " ++ tipo proximoNavio ++ " (tamanho " ++ show (tamanho proximoNavio) ++ ")"
        putStrLn "Digite a coordenada inicial e a orientação (ex: A1 H ou C5 V):"

desenhar assets (EmJogo estadoDoJogo) = do
    limparTela
    let jogadorAtual = turno estadoDoJogo
    putStrLn $ "=================== VEZ DO " ++ show jogadorAtual ++ " ==================="
    putStrLn ""

    putStrLn "Tabuleiro do Oponente (Sua Visão):"
    desenharTabuleiro (ocultarNavios (tabuleiroOponente estadoDoJogo))

    putStrLn "\nSeu Tabuleiro:"
    desenharTabuleiro (tabuleiroJogador estadoDoJogo)

    putStrLn "\nDigite sua jogada (ex: A5):"
      where
        tabuleiroJogador = if turno estadoDoJogo == Jogador1 then tabuleiroP1 estadoDoJogo else tabuleiroP2 estadoDoJogo
        tabuleiroOponente = if turno estadoDoJogo == Jogador1 then tabuleiroP2 estadoDoJogo else tabuleiroP1 estadoDoJogo

desenhar assets (TransicaoDeTurno estadoDoJogo) = do
    limparTela
    let jogadorAnterior = turno estadoDoJogo
    let proximoJogador = if jogadorAnterior == Jogador1 then Jogador2 else Jogador1

    putStrLn (arteTelaTransicao assets)
    putStrLn ""
    putStrLn $ "Turno do " ++ show jogadorAnterior ++ " finalizado."
    putStrLn ""
    putStrLn $ "Passe o controle para o " ++ show proximoJogador ++ " e pressione [Enter] para continuar."

desenhar assets (FimDeJogo estadoFinal) = do
    limparTela
    let vencedor = turno estadoFinal

    case modo estadoFinal of
        MultiPlayer -> do
            putStrLn (arteVitoria assets)
            putStrLn $ "\nO VENCEDOR É: " ++ show vencedor ++ "!"

        SinglePlayer -> do
            if vencedor == Jogador1 then do
                putStrLn (arteVitoria assets)
                putStrLn "\nParabéns pela grande vitória!"
            else do
                putStrLn (arteDerrota assets)
                putStrLn "\nO Computador venceu. Mais sorte na próxima vez."

    putStrLn "\nPressione [Enter] para voltar ao menu."

desenhar _ Saindo = return ()

desenharCelula :: Celula -> IO ()
desenharCelula Agua = do
    setSGR [SetColor Foreground Dull Blue]
    putStr "🌊 "
    setSGR [Reset]
desenharCelula ParteNavio = do
    setSGR [SetColor Foreground Dull White]
    putStr "🚢 "
    setSGR [Reset]
desenharCelula Atingido = do
    setSGR [SetColor Foreground Vivid Red]
    putStr "🔥 "
    setSGR [Reset]
desenharCelula Erro = do
    putStr "❌ "

desenharTabuleiro :: Tabuleiro -> IO ()
desenharTabuleiro tabuleiro = do
    putStr "   "
    mapM_ (\c -> putStr [c] >> putStr "  ") ['A'..tamanhoTabuleiroChar]
    putStrLn ""

    mapM_ (\(numLinha, linha) -> do
        if numLinha < 9 then putStr (" " ++ show (numLinha + 1) ++ " ") else putStr (show (numLinha + 1) ++ " ")
        mapM_ desenharCelula linha
        putStrLn ""
    ) (zip [0..] tabuleiro)
      where
        tamanhoTabuleiroChar = toEnum (fromEnum 'A' + tamanhoTabuleiro - 1)

ocultarNavios :: Tabuleiro -> Tabuleiro
ocultarNavios = map (map esconde)
  where
    esconde :: Celula -> Celula
    esconde ParteNavio = Agua
    esconde celula     = celula