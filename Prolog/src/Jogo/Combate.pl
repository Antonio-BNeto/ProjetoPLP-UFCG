% --- combate.pl ---

:- module(combate, [
    realizar_ataque/6,
    verifica_vitoria/1
]).

% Dependências
:- use_module(tabuleiro, [tabuleiro_obter_celula/3, tabuleiro_marcar_celula/4]).
:- use_module(navio, [
    navio_atualiza_navios/3,
    encontra_navio/3, % renomeado de navio_encontra_navio
    navio_afundado/1,
    navio_get_tipo/2
]).
:- use_module(library(apply), [maplist/2]).

/**
 * realizar_ataque(+TabIn, +NaviosIn, +Coord, -TabOut, -NaviosOut, -Resultado)
 *
 * Processa um ataque na Coordenada.
 * Resultado pode ser: tiro_fora, acerto_repetido, erro_repetido,
 * acertou(Tipo), afundou(Tipo), ou coordenada_invalida.
 */

% Caso 1: A coordenada é inválida (fora do tabuleiro).
realizar_ataque(Tab, Navios, Coord, Tab, Navios, coordenada_invalida) :-
    \+ tabuleiro_obter_celula(Tab, Coord, _), !.

% Caso 2: O ataque atinge uma célula com um estado já definido.
realizar_ataque(Tab, Navios, Coord, Tab, Navios, Resultado) :-
    tabuleiro_obter_celula(Tab, Coord, Estado),
    member(Estado, [atingido, erro]), !, % Se for atingido ou erro
    ( Estado = atingido -> Resultado = acerto_repetido
    ; Estado = erro     -> Resultado = erro_repetido
    ).

% Caso 3: Atingiu a água.
realizar_ataque(TabIn, Navios, Coord, TabOut, Navios, tiro_fora) :-
    tabuleiro_obter_celula(TabIn, Coord, agua), !,
    tabuleiro_marcar_celula(TabIn, Coord, erro, TabOut).

% Caso 4: Atingiu um navio pela primeira vez.
realizar_ataque(TabIn, NaviosIn, Coord, TabOut, NaviosOut, Resultado) :-
    tabuleiro_obter_celula(TabIn, Coord, parte_navio), !,
    tabuleiro_marcar_celula(TabIn, Coord, atingido, TabOut),
    navio_atualiza_navios(NaviosIn, Coord, NaviosOut),
    encontra_navio(Coord, NaviosOut, NavioAtingido),
    navio_get_tipo(NavioAtingido, Tipo),
    (   navio_afundado(NavioAtingido)
    ->  Resultado = afundou(Tipo)
    ;   Resultado = acertou(Tipo)
    ).


/**
 * verifica_vitoria(+Navios)
 *
 * É verdade se todos os navios na lista de navios estão afundados.
 */
verifica_vitoria(Navios) :-
    maplist(navio_afundado, Navios).
