:- module(combate, [
    realizar_ataque/6,
    verifica_vitoria/1
]).

:- use_module('../jogo/tabuleiro').
:- use_module('../jogo/navio').
:- use_module(library(apply), [maplist/2]).

% --- realizar_ataque(+TabIn, +NaviosIn, +Coord, -TabOut, -NaviosOut, -Resultado) ---

% Cláusula 1: A coordenada está fora do tabuleiro (defensivo)
realizar_ataque(Tab, Nav, Coord, Tab, Nav, coordenada_invalida) :-
    \+ tabuleiro_obter_celula(Tab, Coord, _), !.

% Cláusula 2: Jogada repetida (em local já acertado)
realizar_ataque(Tab, Nav, Coord, Tab, Nav, acerto_repetido) :-
    tabuleiro_obter_celula(Tab, Coord, atingido), !.

% Cláusula 3: Jogada repetida (em local já errado)
realizar_ataque(Tab, Nav, Coord, Tab, Nav, erro_repetido) :-
    tabuleiro_obter_celula(Tab, Coord, erro), !.

% Cláusula 4: O tiro acertou a água.
realizar_ataque(TabIn, Navios, Coord, TabOut, Navios, tiro_fora) :-
    tabuleiro_obter_celula(TabIn, Coord, agua), !,
    tabuleiro_marcar_celula(TabIn, Coord, erro, TabOut). % CORRIGIDO: marca como 'erro'

% Cláusula 5: O tiro acertou parte de um navio.
realizar_ataque(TabIn, NaviosIn, Coord, TabOut, NaviosOut, Resultado) :-
    tabuleiro_obter_celula(TabIn, Coord, parte_navio), !,
    tabuleiro_marcar_celula(TabIn, Coord, atingido, TabOut),
    atualiza_navios(Coord, NaviosIn, NaviosOut),
    encontra_navio(Coord, NaviosOut, NavioAtingido),
    navio_get_tipo(NavioAtingido, Tipo),
    (   navio_afundado(NavioAtingido)
    ->  Resultado = afundou(Tipo)     
    ;   Resultado = acertou(Tipo)   
    ).

verifica_vitoria(Navios) :-
    forall(member(Navio, Navios), navio_afundado(Navio)).