
:- module(arte, [
    art_type/1,
    display_art/1,
    default_art/2,
    load_art/2
]).

:- use_module(library(readutil)).
:- use_module(library(filesex)).  % Para verificar existência de arquivos

% --- Tipos de arte ---
art_type(derrota).
art_type(menu).
art_type(tela_jogo).
art_type(transicao).
art_type(vitoria).
art_type(goodbye).

% --- Caminho base da pasta de arte ---
art_directory('Interface/Arte').

% --- Mapeamento de arquivos de arte ---
art_file(derrota, 'derrota_art.txt').
art_file(menu, 'menu_art.txt').
art_file(tela_jogo, 'telaJogoDefault_art.txt').
art_file(transicao, 'telaTransicao_art.txt').
art_file(vitoria, 'vitoria_art.txt').
art_file(goodbye, 'goodbye_art.txt').

% --- Conteúdo padrão caso arquivo não exista ---
default_art(derrota, 'DERROTA!').
default_art(menu, 'BATALHA NAVAL').
default_art(tela_jogo, 'BATALHA NAVAL').
default_art(transicao, 'TRANSICAO').
default_art(vitoria, 'VITORIA!').
default_art(goodbye, 'OBRIGADO POR JOGAR!').

% --- Carrega arte de arquivo (ou retorna padrão) ---
load_art(ArtType, Conteudo) :-
    art_directory(Dir),
    art_file(ArtType, FileName),
    atomic_list_concat([Dir, '/', FileName], Path),
    ( exists_file(Path)
    -> read_file_to_string(Path, Conteudo, [])
    ;  default_art(ArtType, Conteudo)
    ).

% --- Exibe arte ---
display_art(ArtType) :-
    load_art(ArtType, Conteudo),
    clear_console,
    writeln(Conteudo).

% --- Limpa tela (simples) ---
clear_console :-
    (current_prolog_flag(windows, true)
    -> shell('cls')
    ;  shell('clear')
    ).
