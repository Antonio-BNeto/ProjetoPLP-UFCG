% --- posicionamento.pl ---

:- module(logica_posicionamento, [
    gera_corpo_embarcacao/4,
    valida_posicionamento/4,
    marca_navios_no_tabuleiro/3,
    posicionar_navio/3,
    gera_navios/4
]).

:- use_module('../jogo/tabuleiro').
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1]).
:- use_module(library(lists)).

% ================================================================
% COORDENADAS E HELPERS
% ================================================================
coordenada_valida((X,Y)) :-
    integer(X), integer(Y),
    tamanho_tabuleiro(N),
    X >= 0, X < N,
    Y >= 0, Y < N.

% ================================================================
% GERAR CORPO DA EMBARCACAO
% ================================================================
% gera_corpo_embarcacao(+InicioCoord, +Orientacao, +Tamanho, -Posicoes)
% Orientacao = horizontal | vertical
gera_corpo_embarcacao((X,Y), horizontal, Tamanho, Posicoes) :-
    EndX is X + Tamanho - 1,
    findall((Xi,Y), between(X, EndX, Xi), Posicoes).
gera_corpo_embarcacao((X,Y), vertical, Tamanho, Posicoes) :-
    EndY is Y + Tamanho - 1,
    findall((X,Yi), between(Y, EndY, Yi), Posicoes).

% ================================================================
% VALIDA POSICIONAMENTO
% ================================================================
% valida_posicionamento(+Posicoes, +TabIn, +_Acc, -Valido)
% Valida se as posições estão dentro do tabuleiro e não colidem com outros navios.
valida_posicionamento(Posicoes, TabIn, _Acc, true) :-
    forall(member(Coord, Posicoes),
           (   coordenada_valida(Coord),
               tabuleiro_obter_celula(TabIn, Coord, Valor),
               Valor \= parte_navio
           )).
valida_posicionamento(_Posicoes, _TabIn, _Acc, false).

% ================================================================
% POSICIONAR UM NAVIO (procura primeira posição válida)
% ================================================================
% posicionar_navio(+NavIn, +TabIn, -NavOut)
% NavIn esperado no formato navio(Tipo, Tamanho, [], [])
posicionar_navio(navio(Tipo, Tamanho, [], []), TabIn, navio(Tipo, Posicoes, [])) :-
    tamanho_tabuleiro(N),
    Max is N - 1,
    between(0, Max, X),
    between(0, Max, Y),
    member(Orient, [horizontal, vertical]),
    gera_corpo_embarcacao((X,Y), Orient, Tamanho, Posicoes),
    valida_posicionamento(Posicoes, TabIn, [], true),
    !.

% ================================================================
% MARCAR NAVIOS NO TABULEIRO
% ================================================================
% marca_navios_no_tabuleiro(+ListaNavios, +TabIn, -TabOut)
% Marca todas as posicoes de cada navio como parte_navio.
marca_navios_no_tabuleiro(Navios, TabIn, TabOut) :-
    foldl(marca_navio_no_tabuleiro, Navios, TabIn, TabOut).

marca_navio_no_tabuleiro(navio(_Tipo, Posicoes, _Partes), TabA, TabB) :-
    foldl(marcar_posicao_com_parte, Posicoes, TabA, TabB).

marcar_posicao_com_parte(Pos, TabIn, TabOut) :-
    tabuleiro_marcar_celula(TabIn, Pos, parte_navio, TabOut).

% ================================================================
% GERAR LISTA DE NAVIOS
% ================================================================
% gera_navios(+NaviosBase, +TabIn, -NaviosPosicionados, -TabOut)
gera_navios([], Tab, [], Tab).
gera_navios([navio(Tipo, Tamanho, [], [])|Resto], TabIn, [NavioPos|NaviosOut], TabOut) :-
    posicionar_navio(navio(Tipo, Tamanho, [], []), TabIn, NavioPos),
    marca_navios_no_tabuleiro([NavioPos], TabIn, TabTemp),
    gera_navios(Resto, TabTemp, NaviosOut, TabOut).
