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


coordenada_valida((X,Y)) :-
    integer(X), integer(Y),
    tamanho_tabuleiro(N),
    X >= 0, X < N,
    Y >= 0, Y < N.


gera_corpo_embarcacao((X,Y), horizontal, Tamanho, Posicoes) :-
    EndX is X + Tamanho - 1,
    findall((Xi,Y), between(X, EndX, Xi), Posicoes).
gera_corpo_embarcacao((X,Y), vertical, Tamanho, Posicoes) :-
    EndY is Y + Tamanho - 1,
    findall((X,Yi), between(Y, EndY, Yi), Posicoes).

valida_posicionamento(Posicoes, TabIn, _Acc, true) :-
    forall(member(Coord, Posicoes),
           (   coordenada_valida(Coord),
               tabuleiro_obter_celula(TabIn, Coord, Valor),
               Valor \= parte_navio
           )).
valida_posicionamento(_Posicoes, _TabIn, _Acc, false).


gera_orientacao_aleatoria(Orient) :-
    random_between(0, 1, I),
    nth0(I, [horizontal, vertical], Orient).

posicionar_navio(navio(Tipo, Tamanho, [], []), TabIn, navio(Tipo, Posicoes, [])) :-
    tamanho_tabuleiro(N),
    Max is N - 1,
    repeat, 
        
        random_between(0, Max, X),
        random_between(0, Max, Y),
        gera_orientacao_aleatoria(Orient),

      
        gera_corpo_embarcacao((X,Y), Orient, Tamanho, Posicoes),
        valida_posicionamento(Posicoes, TabIn, [], true),
    !. 



marca_navios_no_tabuleiro(Navios, TabIn, TabOut) :-
    foldl(marca_navio_no_tabuleiro, Navios, TabIn, TabOut).

marca_navio_no_tabuleiro(navio(_Tipo, Posicoes, _Partes), TabA, TabB) :-
    foldl(marcar_posicao_com_parte, Posicoes, TabA, TabB).

marcar_posicao_com_parte(Pos, TabIn, TabOut) :-
    tabuleiro_marcar_celula(TabIn, Pos, parte_navio, TabOut).


gera_navios([], Tab, [], Tab).
gera_navios([navio(Tipo, Tamanho, [], [])|Resto], TabIn, [NavioPos|NaviosOut], TabOut) :-
    posicionar_navio(navio(Tipo, Tamanho, [], []), TabIn, NavioPos),
    marca_navios_no_tabuleiro([NavioPos], TabIn, TabTemp),
    gera_navios(Resto, TabTemp, NaviosOut, TabOut).