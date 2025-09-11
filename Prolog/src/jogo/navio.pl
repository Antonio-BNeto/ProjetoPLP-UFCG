% --- navio.pl ---

:- module(navio, [
    navio_afundado/1,
    encontra_navio/3,
    atualiza_navios/3,
    navio_get_tipo/2,
    navio_get_posicoes/2,
    tamanho/2
]).


:- use_module(library(lists)).

% Estrutura unificada: navio(Tipo, Posicoes, PartesAtingidas)
% Ex: navio(pequeno, [(0,0), (0,1)], [(0,0)]).

% --- Getters ---
navio_get_tipo(navio(Tipo, _, _), Tipo).
navio_get_posicoes(navio(_, Posicoes, _), Posicoes).
navio_get_partes_atingidas(navio(_, _, PartesAtingidas), PartesAtingidas).


% set_posicoes(+NavioIn, +Posicoes, -NavioOut)
set_posicoes(navio(Tipo, _, PartesAtingidas), Posicoes, navio(Tipo, Posicoes, PartesAtingidas)).


% --- Lógica Principal ---

% navio_afundado(+Navio)
% Verdadeiro se todas as posições do navio foram atingidas.
navio_afundado(Navio) :-
    navio_get_posicoes(Navio, Posicoes),
    navio_get_partes_atingidas(Navio, PartesAtingidas),
    subset(Posicoes, PartesAtingidas).

% encontra_navio(+Coord, +ListaNavios, -NavioEncontrado)
% Encontra o navio que ocupa a Coordenada. Falha se não houver.
encontra_navio(Coord, [Navio|_], Navio) :-
    navio_get_posicoes(Navio, Posicoes),
    member(Coord, Posicoes), !.
encontra_navio(Coord, [_|RestoNavios], NavioEncontrado) :-
    encontra_navio(Coord, RestoNavios, NavioEncontrado).

% atualiza_navios(+Coord, +NaviosIn, -NaviosOut)
% Adiciona a Coordenada às partes atingidas do navio correspondente.
atualiza_navios(_, [], []).
atualiza_navios(Coord, [navio(Tipo, Pos, PartesIn)|Resto], [navio(Tipo, Pos, [Coord|PartesIn])|Resto]) :-
    member(Coord, Pos),
    \+ member(Coord, PartesIn), !.
atualiza_navios(Coord, [Navio|RestoIn], [Navio|RestoOut]) :-
    atualiza_navios(Coord, RestoIn, RestoOut).


    
% --- Tamanho de cada tipo de navio ---
tamanho(porta_avioes, 5).
tamanho(encouracado, 4).
tamanho(cruzador, 3).
tamanho(destroyer, 3).
tamanho(submarino, 2).
