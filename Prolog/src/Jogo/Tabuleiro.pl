% --- tabuleiro.pl ---

:- module(tabuleiro, [
    tabuleiro_obter_celula/3,
    tabuleiro_marcar_celula/4
]).

% Dependências
:- use_module(lista, [atualiza_indice/4]).
:- use_module(library(lists), [nth0/3]). % Predicado nativo para acesso a listas


/**
 * tabuleiro_obter_celula(+Tabuleiro, +Coordenada, -Celula)
 *
 * Obtém o estado da Celula na Coordenada (Linha, Coluna) do Tabuleiro.
 * Falha se a coordenada for inválida (fora dos limites).
 */
tabuleiro_obter_celula(Tabuleiro, (X, Y), Celula) :-
    nth0(X, Tabuleiro, Linha),
    nth0(Y, Linha, Celula).

/**
 * tabuleiro_marcar_celula(+TabIn, +Coordenada, +NovaCelula, -TabOut)
 *
 * Cria um novo tabuleiro (TabOut) marcando a célula na
 * Coordenada (X,Y) de TabIn com o valor de NovaCelula.
 */
tabuleiro_marcar_celula(TabIn, (X, Y), NovaCelula, TabOut) :-
    nth0(X, TabIn, LinhaOriginal),
    atualiza_indice(Y, NovaCelula, LinhaOriginal, NovaLinha),
    atualiza_indice(X, NovaLinha, TabIn, TabOut).
