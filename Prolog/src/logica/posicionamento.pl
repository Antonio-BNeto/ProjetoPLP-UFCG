:- module(logica_posicionamento, [
    gera_corpo_embarcacao/4,
    valida_posicionamento/4,
    marca_navios_no_tabuleiro/3,
    posicionar_navio/3
]).

:- use_module('../jogo/tabuleiro').

% ============================================
% REGRAS PRINCIPAIS
% ============================================

coordenada_valida((X,Y)) :-
    tamanho_tabuleiro(T),
    X >= 0, Y >= 0,
    X < T, Y < T.

% Gera corpo do navio
gera_corpo_embarcacao((X,Y), Navio, h, Corpo) :-
    tamanho(Navio, Tam),
    T1 is Tam-1,
    findall((X1,Y),
        (between(0, T1, I), X1 is X+I),
        Corpo).

gera_corpo_embarcacao((X,Y), Navio, v, Corpo) :-
    tamanho(Navio, Tam),
    T1 is Tam-1,
    findall((X,Y1),
        (between(0, T1, I), Y1 is Y+I),
        Corpo).

% Valida se pode posicionar navio
valida_posicionamento(Tab, Coord, Navio, Orient) :-
    gera_corpo_embarcacao(Coord, Navio, Orient, Corpo),
    maplist(coordenada_valida, Corpo),
    maplist(celula_livre(Tab), Corpo).

celula_livre(Tab, Coord) :- obter(Tab, Coord, agua).

% Marca navios no tabuleiro
marca_navios_no_tabuleiro(Tab, [], Tab).
marca_navios_no_tabuleiro(Tab, [C|Cs], NovoTab) :-
    marca(Tab, C, parte_navio, Tab1),
    marca_navios_no_tabuleiro(Tab1, Cs, NovoTab).

% Posiciona navio até sucesso
posicionar_navio(Tab, Navio, (NovoNavio, NovoTab)) :-
    gera_coordenada(Pos),
    gera_orientacao(Orient),
    ( valida_posicionamento(Tab, Pos, Navio, Orient) ->
        gera_corpo_embarcacao(Pos, Navio, Orient, Corpo),
        marca_navios_no_tabuleiro(Tab, Corpo, NovoTab),
        set_posicoes(Navio, Corpo, NovoNavio)
    ; posicionar_navio(Tab, Navio, (NovoNavio, NovoTab))
    ).
