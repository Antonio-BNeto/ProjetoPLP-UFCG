:- module(logica_posicionamento, [
    gera_corpo_embarcacao/4,
    valida_posicionamento/4,
    marca_navios_no_tabuleiro/3,
    posicionar_navio/3
]).

:- use_module('../jogo/tabuleiro').
:- use_module('../jogo/arquitetura').
:- use_module('../jogo/navio').
:- use_module(library(random)).

% ============================================
% COORDENADAS VÁLIDAS
% ============================================

coordenada_valida((X,Y)) :-
    tamanho_tabuleiro(T),
    integer(T),
    X >= 0, Y >= 0,
    X < T, Y < T.

celula_livre(Tab, Coord) :-
    obter(Tab, Coord, agua).

% ============================================
% GERAÇÃO DE CORPO DO NAVIO
% ============================================

gera_corpo_embarcacao((X,Y), Navio, h, Corpo) :-
    tamanho(Navio, Tam),
    integer(Tam),
    T1 is Tam - 1,
    findall((X1,Y), (between(0, T1, I), X1 is X + I), Corpo).

gera_corpo_embarcacao((X,Y), Navio, v, Corpo) :-
    tamanho(Navio, Tam),
    integer(Tam),
    T1 is Tam - 1,
    findall((X,Y1), (between(0, T1, I), Y1 is Y + I), Corpo).

% ============================================
% VALIDAÇÃO DE POSICIONAMENTO
% ============================================

valida_posicionamento(Tab, Coord, Navio, Orient) :-
    gera_corpo_embarcacao(Coord, Navio, Orient, Corpo),
    maplist(coordenada_valida, Corpo),
    maplist(celula_livre(Tab), Corpo).

% ============================================
% MARCAR NAVIO NO TABULEIRO
% ============================================

marca_navios_no_tabuleiro(Tab, [], Tab).
marca_navios_no_tabuleiro(Tab, [C|Cs], NovoTab) :-
    marca(Tab, C, parte_navio, Tab1),
    marca_navios_no_tabuleiro(Tab1, Cs, NovoTab).

% ============================================
% POSICIONAMENTO DE NAVIO
% ============================================

posicionar_navio(Tab, Navio, (NovoNavio, NovoTab)) :-
    gera_orientacao(Orient),
    gera_coordenada_ajustada(Navio, Orient, Pos),
    ( valida_posicionamento(Tab, Pos, Navio, Orient) ->
        gera_corpo_embarcacao(Pos, Navio, Orient, Corpo),
        marca_navios_no_tabuleiro(Tab, Corpo, NovoTab),
        set_posicoes(Navio, Corpo, NovoNavio)
    ;   % Se a posição não é válida, tenta novamente recursivamente
        posicionar_navio(Tab, Navio, (NovoNavio, NovoTab))
    ).

% ============================================
% GERADORES AUXILIARES
% ============================================

% Gera orientação aleatória
gera_orientacao(Orient) :-
    random_member(Orient, [h, v]).

% Gera coordenada dentro do tabuleiro garantindo que o navio caiba
gera_coordenada_ajustada(Navio, Orient, (X,Y)) :-
    tamanho_tabuleiro(T),
    integer(T),
    tamanho(Navio, Tam),
    integer(Tam),
    ( Orient = h -> MaxX0 is T - Tam, MaxY0 is T - 1
    ; Orient = v -> MaxX0 is T - 1, MaxY0 is T - Tam
    ),
    MaxX is integer(MaxX0),
    MaxY is integer(MaxY0),
    random_between(0, MaxX, X),
    random_between(0, MaxY, Y).
