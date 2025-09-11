% --- navio.pl ---

:- module(navio, [
    navio_afundado/1,
    encontra_navio/3,
    atualiza_navios/3,
    navio_get_tipo/2,
    navio_get_posicoes/2
]).

:- use_module(library(lists)).

% Estrutura unificada: navio(Tipo, Posicoes, PartesAtingidas)
% Ex: navio(pequeno, [(0,0), (0,1)], [(0,0)]).

% --- Getters ---
navio_get_tipo(navio(Tipo, _Pos, _Partes), Tipo).
navio_get_posicoes(navio(_Tipo, Posicoes, _Partes), Posicoes).

% navio_afundado(+Navio)
% Verdadeiro se todas as posicoes do navio estiverem entre as partes atingidas.
navio_afundado(navio(_Tipo, Posicoes, Partes)) :-
    forall(member(P, Posicoes), member(P, Partes)).

% Encontra o navio que ocupa a Coordenada. Falha se não houver.
encontra_navio(Coord, [Navio|_], Navio) :-
    navio_get_posicoes(Navio, Posicoes),
    member(Coord, Posicoes), !.
encontra_navio(Coord, [_|RestoNavios], NavioEncontrado) :-
    encontra_navio(Coord, RestoNavios, NavioEncontrado).

% atualiza_navios(+Coord, +NaviosIn, -NaviosOut)
% Adiciona a Coordenada às partes atingidas do navio correspondente.
atualiza_navios(_, [], []).
atualiza_navios(Coord, [navio(Tipo, Pos, PartesIn)|Resto], [navio(Tipo, Pos, PartesOut)|Resto]) :-
    member(Coord, Pos),
    (   member(Coord, PartesIn)
    ->  % já constava como atingido, mantém
        PartesOut = PartesIn
    ;   % acrescenta coordenada às partes atingidas
        PartesOut = [Coord|PartesIn]
    ), !.
atualiza_navios(Coord, [Navio|RestoIn], [Navio|RestoOut]) :-
    atualiza_navios(Coord, RestoIn, RestoOut).
