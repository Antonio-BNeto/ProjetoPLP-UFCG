module Interface.Arte where

import System.Console.ANSI (clearScreen)
import System.FilePath ((</>))
import System.Directory (doesFileExist)

data ArtType = Derrota | Menu | Jogo | Transicao | Vitoria | Adeus
    deriving (Show, Eq)

-- Nomes dos arquivos de arte
artFileName :: ArtType -> FilePath
artFileName art = case art of
    Derrota    -> "derrota_art.txt"
    Menu       -> "menu_art.txt"
    Jogo       -> "jogo_art.txt"
    Vitoria    -> "vitoria_art.txt"
    Adeus      -> "adeus_art.txt"
    Transicao  -> "" -- Transição pode não ter arte

-- Carrega a arte usando caminho relativo fixo
loadArt :: ArtType -> IO String
loadArt artType = do
    
    let path = "src" </> "Interface" </> "Arte_ascii" </> artFileName artType

    exists <- doesFileExist path
    if exists
        then readFile path
        else do
            putStrLn $ "Aviso: Arquivo de arte não encontrado em '" ++ path ++ "'. Usando arte padrão."
            return $ defaultArt artType

-- Arte padrão caso o arquivo não exista
defaultArt :: ArtType -> String
defaultArt artType = case artType of
    Derrota   -> "DERROTA!"
    Menu      -> "BATALHA NAVAL"
    Jogo      -> "BATALHA NAVAL"
    Vitoria   -> "VITORIA!"
    Adeus     -> "OBRIGADO POR JOGAR!"
    Transicao -> "Carregando..."

-- Exibe arte com limpeza de tela
displayArt :: ArtType -> IO ()
displayArt artType = do
    clearScreen
    if artType == Transicao
        then putStrLn (defaultArt Transicao)
        else loadArt artType >>= putStrLn
