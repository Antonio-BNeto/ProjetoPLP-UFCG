:- module(arte, [
    display_art/1,
    main/0,
    clear_screen/0
]).

:- use_module(library(readutil)).

% --- CONFIGURAÇÃO CORRIGIDA ---
art_directory('Arte_ascii').  % ← MUDEI PARA Arte_ascii

% Mapeamento dos arquivos
art_file(adeus,   'adeus_art.txt').
art_file(derrota, 'derrota_art.txt').
art_file(jogo,    'jogo_art.txt').
art_file(menu,    'menu_art.txt').
art_file(vitoria, 'vitoria_art.txt').

% --- PREDICADOS PRINCIPAIS ---
display_art(ArtType) :-
    load_art(ArtType, Conteudo),
    clear_screen,
    write(Conteudo).

main :-
    forall(art_file(Type, _), 
           (display_art(Type), 
            nl, nl,
            sleep(3))).

% --- CARREGAMENTO DE ARQUIVOS ---
load_art(ArtType, Conteudo) :-
    art_directory(Dir),
    art_file(ArtType, FileName),
    atomic_list_concat([Dir, '/', FileName], Path),
    (   exists_file(Path)
    ->  read_file_to_string(Path, Conteudo, [])
    ;   default_art(ArtType, Conteudo)
    ).

% --- LIMPEZA DE TELA ---
clear_screen :-
    (   current_prolog_flag(windows, true)
    ->  shell('cls')
    ;   shell('clear')
    ).

% --- TEXTOS PADRÃO ---
default_art(adeus,   'OBRIGADO POR JOGAR!').
default_art(derrota, 'QUE PENA, VOCÊ PERDEU!').
default_art(jogo,    '--- BATALHA NAVAL ---').
default_art(menu,    '--- MENU PRINCIPAL ---').
default_art(vitoria, 'PARABÉNS, VOCÊ VENCEU!').