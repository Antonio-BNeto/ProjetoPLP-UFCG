% --- lista.pl (VERSÃO MELHORADA E CENTRALIZADA) ---

:- module(lista, [
    atualiza_indice/4
]).

/* atualiza_indice(+Indice, +NovoElemento, +ListaIn, -ListaOut)
 *
 * É verdade quando ListaOut é o resultado da substituição do elemento
 * no Indice (base 0) de ListaIn pelo NovoElemento.
 *
 * Esta implementação usa nth0/4, que é eficiente e falha se o
 * Indice estiver fora dos limites da lista, tornando o código mais robusto.
 */
atualiza_indice(Indice, NovoElemento, ListaIn, ListaOut) :-
    nth0(Indice, ListaIn, _, Restante),
    nth0(Indice, ListaOut, NovoElemento, Restante).
