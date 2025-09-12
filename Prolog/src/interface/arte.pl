:- module(arte, [
    display_art/1,
    clear_screen/0
]).

:- use_module(library(readutil)).
:- use_module(library(filesex)).


art_directory('src/interface/arte_ascii').


art_file(menu,       'menu_art.txt').
art_file(preparacao, 'preparacao_art.txt').
art_file(jogo,       'jogo_art.txt').
art_file(vitoria,    'vitoria_art.txt').
art_file(derrota,    'derrota_art.txt').
art_file(adeus,      'adeus_art.txt').


display_art(ArtType) :-
    load_art(ArtType, Conteudo),
    clear_screen,
    format("~s~n", [Conteudo]).


load_art(ArtType, Conteudo) :-
    art_directory(Dir),
    art_file(ArtType, FileName),
    atomic_list_concat([Dir, '/', FileName], Path),
    (   exists_file(Path)
    ->  read_file_to_string(Path, Conteudo, [])
    ;   default_art(ArtType, Conteudo)
    ).

clear_screen :-
    (   current_prolog_flag(windows, true)
    ->  shell('cls')
    ;   shell('clear')
    ).


default_art(menu,       '=== MENU BATALHA NAVAL ===').
default_art(preparacao, 'Preparando tabuleiros...').
default_art(jogo,       'Batalha Naval em andamento!').
default_art(vitoria,    'PARABÉNS! Você venceu!').
default_art(derrota,    'Você perdeu. Tente novamente!').
default_art(adeus,      'Obrigado por jogar!').
