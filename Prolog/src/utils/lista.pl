
:- module(lista, [
    atualiza_indice/4
]).

atualiza_indice(Indice, NovoElemento, ListaIn, ListaOut) :-
    nth0(Indice, ListaIn, _, Restante),
    nth0(Indice, ListaOut, NovoElemento, Restante).
