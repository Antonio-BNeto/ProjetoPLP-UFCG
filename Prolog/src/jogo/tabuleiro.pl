% --- tabuleiro.pl (ATUALIZADO PARA USAR O MÓDULO 'lista') ---

:- module(tabuleiro, [
    tabuleiro_obter_celula/3,
    tabuleiro_marcar_celula/4
]).

:- use_module('../utils/lista'). % importa atualiza_indice/4
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1]).

% =================================================================
% === LÓGICA PARA TABULEIRO ESPARSO (usado pelo Bot)
% Representação esparsa: lista de pares (Coord, Valor)
% Coord representado como (X,Y) — mesmo formato usado em todo o projeto.
% =================================================================

obter_celula_esparso(Tabuleiro, Coord, Valor) :-
    (   member((Coord, V), Tabuleiro)
    ->  Valor = V
    ;   % se não encontrado, assumimos 'agua' por convenção
        Valor = agua
    ).

marcar_celula_esparso(TabIn, Coord, Valor, TabOut) :-
    (   select((Coord, _Old), TabIn, Rest)
    ->  TabOut = [(Coord, Valor)|Rest]
    ;   TabOut = [(Coord, Valor)|TabIn]
    ).

% =================================================================
% === LÓGICA PARA TABULEIRO DENSO (matriz: lista de linhas)
% =================================================================

obter_celula_denso(Tabuleiro, (X,Y), Valor) :-
    nth0(X, Tabuleiro, Linha),
    nth0(Y, Linha, Valor).

marcar_celula_denso(TabIn, (X,Y), Valor, TabOut) :-
    % obtém linha X, substitui posição Y e reconstrói TabOut
    nth0(X, TabIn, LinhaIn, RestoLinhas),
    nth0(Y, LinhaIn, _Old, RestoElementos),
    nth0(Y, LinhaOut, Valor, RestoElementos),
    nth0(X, TabOut, LinhaOut, RestoLinhas).

% =================================================================
% === PREDICADOS PRINCIPAIS (com detecção de formato)
% =================================================================

tabuleiro_obter_celula(Tab, Coord, Valor) :-
    (   Tab = [H|_], is_list(H)
    ->  obter_celula_denso(Tab, Coord, Valor)
    ;   obter_celula_esparso(Tab, Coord, Valor)
    ).

tabuleiro_marcar_celula(TabIn, Coord, Valor, TabOut) :-
    (   TabIn = [H|_], is_list(H)
    ->  marcar_celula_denso(TabIn, Coord, Valor, TabOut)
    ;   marcar_celula_esparso(TabIn, Coord, Valor, TabOut)
    ).
