:- module(logica_posicionamento, [
    gera_corpo_embarcacao/4,
    valida_posicionamento/4,
    marca_navios_no_tabuleiro/3,
    posicionar_navio/3
    coordenada_valida/1,        % <-- exporte
    tamanho_tabuleiro/1          % <-- exporte também
]).

% Importa tamanho_tabuleiro de Arquitetura
:- use_module('../jogo/Arquitetura.pl', [tamanho_tabuleiro/1]).

% ============================================
% DEFINIÇÕES AUXILIARES (mock simplificado)
% ============================================

tamanho(navio(pequeno,_), 2).
tamanho(navio(medio,_), 3).
tamanho(navio(grande,_), 4).

set_posicoes(navio(Tipo,_), Pos, navio(Tipo,Pos)).

celula(agua).
celula(parte_navio).
celula(atingido).
celula(erro).

obter(Tab, Coord, Valor) :-
    ( member((Coord,V), Tab) -> Valor = V ; Valor = agua ).

marca(Tab, Coord, Valor, NovoTab) :-
    select((Coord,_), Tab, Restante) -> NovoTab = [(Coord,Valor)|Restante]
    ; NovoTab = [(Coord,Valor)|Tab].

gera_coordenada((X,Y)) :-
    tamanho_tabuleiro(T),
    Max is T-1,
    random_between(0, Max, X),
    random_between(0, Max, Y).

gera_orientacao(h) :-
    random_between(0, 1, V),
    V =:= 0, !.
gera_orientacao(v).

coordenada_valida((X,Y)) :-
    tamanho_tabuleiro(T),
    X >= 0, Y >= 0,
    X < T, Y < T.

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

valida_posicionamento(Tab, Coord, Navio, Orient) :-
    gera_corpo_embarcacao(Coord, Navio, Orient, Corpo),
    maplist(coordenada_valida, Corpo),
    maplist(celula_livre(Tab), Corpo).

celula_livre(Tab, Coord) :- obter(Tab, Coord, agua).

marca_navios_no_tabuleiro(Tab, [], Tab).
marca_navios_no_tabuleiro(Tab, [C|Cs], NovoTab) :-
    marca(Tab, C, parte_navio, Tab1),
    marca_navios_no_tabuleiro(Tab1, Cs, NovoTab).

posicionar_navio(Tab, Navio, (NovoNavio, NovoTab)) :-
    gera_coordenada(Pos),
    gera_orientacao(Orient),
    ( valida_posicionamento(Tab, Pos, Navio, Orient) ->
        gera_corpo_embarcacao(Pos, Navio, Orient, Corpo),
        marca_navios_no_tabuleiro(Tab, Corpo, NovoTab),
        set_posicoes(Navio, Corpo, NovoNavio)
    ; posicionar_navio(Tab, Navio, (NovoNavio, NovoTab))
    ).
