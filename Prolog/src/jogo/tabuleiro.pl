% --- tabuleiro.pl (ATUALIZADO PARA USAR O MÓDULO 'lista') ---

:- module(tabuleiro, [
    tabuleiro_obter_celula/3,
    tabuleiro_marcar_celula/4
]).

:- use_module(lista). % <-- ADICIONADO: Importa o módulo centralizado.

% =================================================================
% === LÓGICA PARA TABULEIRO ESPARSO (usado pelo Bot)
% =================================================================

obter_celula_esparso(Tabuleiro, Coord, Valor) :-
    (   member((Coord, V), Tabuleiro)
    ->  Valor = V
    ;   Valor = agua
    ).

marcar_celula_esparso(TabIn, Coord, Valor, TabOut) :-
    (   select((Coord, _), TabIn, Restante)
    ->  TabOut = [(Coord, Valor)|Restante]
    ;   TabOut = [(Coord, Valor)|TabIn]
    ).

% =================================================================
% === LÓGICA PARA TABULEIRO DENSO (usado pelo Jogador)
% =================================================================

% --- O PREDICADO 'replace_nth/4' FOI REMOVIDO DAQUI ---

obter_celula_denso(Tabuleiro, (X,Y), Valor) :-
    nth0(Y, Tabuleiro, Linha),
    nth0(X, Linha, Valor).

marcar_celula_denso(TabIn, (X,Y), Valor, TabOut) :-
    nth0(Y, TabIn, LinhaAntiga),
    % --- ALTERADO: Agora chama o predicado do módulo 'lista' ---
    atualiza_indice(X, Valor, LinhaAntiga, LinhaNova),
    atualiza_indice(Y, LinhaNova, TabIn, TabOut).

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