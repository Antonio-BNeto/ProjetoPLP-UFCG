module Interface.Arte where

import System.IO.Error (tryIOError)
import System.Console.ANSI (clearScreen)
import System.FilePath ((</>))  -- Para combinar caminhos de forma segura
import System.Directory (getCurrentDirectory)

-- Adicionei Goodbye no tipo ArtType
data ArtType = Derrota | Menu | TelaJogo | Transicao | Vitoria | Goodbye

-- Caminho base relativo ao executável
artDirectory :: FilePath
artDirectory = "Arte"

-- Função segura para construir caminhos (atualizada com Goodbye)
artPaths :: ArtType -> FilePath
artPaths art = artDirectory </> case art of
    Derrota    -> "derrota_art.txt"
    Menu       -> "menu_art.txt"
    TelaJogo   -> "telaJogoDefault_art.txt"
    Transicao  -> "telaTransicao_art.txt"
    Vitoria    -> "vitoria_art.txt"
    Goodbye    -> "goodbye_art.txt"  -- Novo caso para Goodbye

-- Versão robusta para carregar arte
loadArt :: ArtType -> IO String
loadArt artType = do
    currentDir <- getCurrentDirectory
    let artPath = artPaths artType
    
    -- Tenta ler o arquivo de 3 formas diferentes:
    result <- tryIOError (readFile artPath)  -- 1. Diretamente
    case result of
        Right content -> return content
        Left _ -> do
            -- 2. Tentando com caminho relativo ao executável
            let exeRelativePath = currentDir </> artPath
            resultExe <- tryIOError (readFile exeRelativePath)
            case resultExe of
                Right content -> return content
                Left _ -> do
                    -- 3. Tentando com caminho absoluto (último recurso)
                    let absolutePath = currentDir </> "src" </> artPath
                    resultAbs <- tryIOError (readFile absolutePath)
                    case resultAbs of
                        Right content -> return content
                        Left err -> do
                            putStrLn $ "Erro ao carregar arte (" ++ artPath ++ "): " ++ show err
                            return $ defaultArt artType

-- Arte padrão caso o arquivo não exista (atualizada com Goodbye)
defaultArt :: ArtType -> String
defaultArt artType = case artType of
    Derrota  -> "DERROTA!"
    Menu     -> "BATALHA NAVAL"
    TelaJogo -> "BATALHA NAVAL"
    Transicao -> "TRANSICAO"
    Vitoria  -> "VITORIA!"
    Goodbye  -> "OBRIGADO POR JOGAR!"  -- Mensagem padrão para Goodbye

-- Exibe arte com limpeza de tela
displayArt :: ArtType -> IO ()
displayArt artType = do
    clearScreen
    art <- loadArt artType
    putStrLn art