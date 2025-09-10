% --- combate.pl ---

:- module(combate, [
    realizar_ataque/6,
    verifica_vitoria/1
]).

:- use_module(tabuleiro, [tabuleiro_obter_celula/3, tabuleiro_marcar_celula/4]).
:- use_module(navio, [
    atualiza_navios/3,
    encontra_navio/3,
    navio_afundado/1,
    navio_get_tipo/2
]).
:- use_module(library(apply), [maplist/2]).

% realizar_ataque(+TabIn, +NaviosIn, +Coord, -TabOut, -NaviosOut, -Resultado)

% Cláusula 1: A coordenada está fora do tabuleiro.
realizar_ataque(Tab, Navios, Coord, Tab, Navios, coordenada_invalida) :-
    \+ tabuleiro_obter_celula(Tab, Coord, _), !.

% Cláusula 2: O jogador já atirou nesta coordenada antes.
realizar_ataque(Tab, Navios, Coord, Tab, Navios, Resultado) :-
    tabuleiro_obter_celula(Tab, Coord, Estado),
    member(Estado, [atingido, erro]), !,
    ( Estado = atingido -> Resultado = acerto_repetido
    ; Estado = erro     -> Resultado = erro_repetido
    ).

% Cláusula 3: O tiro acertou a água.
% --- ALTERAÇÃO REALIZADA AQUI ---
% Agora verifica se o estado da célula é '🌊' (tabuleiro do jogador)
% OU 'agua' (padrão do tabuleiro do bot), tornando a lógica compatível com ambos.
realizar_ataque(TabIn, Navios, Coord, TabOut, Navios, tiro_fora) :-
    tabuleiro_obter_celula(TabIn, Coord, Estado),
    member(Estado, ['🌊', agua]), !,
    tabuleiro_marcar_celula(TabIn, Coord, erro, TabOut).

% Cláusula 4: O tiro acertou parte de um navio.
realizar_ataque(TabIn, NaviosIn, Coord, TabOut, NaviosOut, Resultado) :-
    tabuleiro_obter_celula(TabIn, Coord, parte_navio), !,
    tabuleiro_marcar_celula(TabIn, Coord, atingido, TabOut),
    atualiza_navios(NaviosIn, Coord, NaviosOut),
    encontra_navio(Coord, NaviosOut, NavioAtingido),
    navio_get_tipo(NavioAtingido, Tipo),
    (   navio_afundado(NavioAtingido)
    ->  Resultado = afundou(Tipo)
    ;   Resultado = acertou(Tipo)
    ).

% verifica_vitoria(+Navios)
% Verdadeiro se todos os navios na lista estiverem afundados.
verifica_vitoria(Navios) :-
    maplist(navio_afundado, Navios).