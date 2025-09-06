% --- lista.pl ---

:- module(lista, [
    atualiza_indice/4
]).

/**
 * atualiza_indice(+Indice, +NovoElemento, +ListaIn, -ListaOut)
 *
 * É verdade quando ListaOut é o resultado da substituição do elemento
 * no Indice (base 0) de ListaIn pelo NovoElemento.
 */

% Caso base: O índice é 0, substituímos a cabeça da lista.
atualiza_indice(0, Novo, [_|Cauda], [Novo|Cauda]).

% Passo recursivo: O índice N é maior que 0.
atualiza_indice(N, Novo, [Cabeca|CaudaIn], [Cabeca|CaudaOut]) :-
    N > 0,
    N1 is N - 1,
    atualiza_indice(N1, Novo, CaudaIn, CaudaOut).

% Caso de borda: A lista de entrada está vazia (índice fora dos limites).
atualiza_indice(_, _, [], []).
