% --- navio.pl ---

:- module(navio, [
    navio_afundado/1,
    encontra_navio/3,
    atualiza_navios/3,
    navio_get_tipo/2,
    navio_get_tamanho/2,
    navio_get_posicoes/2,
    navio_get_partes_atingidas/2
]).

:- use_module(library(lists)). % para member/2 e subset/2

% Estrutura do navio: navio(Tipo, Tamanho, Posicoes, PartesAtingidas)

% --- Getters (para acessar os dados da estrutura de forma segura) ---
navio_get_tipo(navio(Tipo, _, _, _), Tipo).
navio_get_tamanho(navio(_, Tamanho, _, _), Tamanho).
navio_get_posicoes(navio(_, _, Posicoes, _), Posicoes).
navio_get_partes_atingidas(navio(_, _, _, PartesAtingidas), PartesAtingidas).


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
atualiza_navios(Coord, [navio(T, Tam, Pos, PartesIn)|Resto], [navio(T, Tam, Pos, [Coord|PartesIn])|Resto]) :-
    member(Coord, Pos),
    \+ member(Coord, PartesIn), !.
atualiza_navios(Coord, [Navio|RestoIn], [Navio|RestoOut]) :-
    atualiza_navios(Coord, RestoIn, RestoOut).
