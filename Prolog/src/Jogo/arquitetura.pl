:- module(jogo_arquitetura, [
    celula/1,
    coordenada/2,
    criacao_tabuleiro/1,
    exibicao_celula/2,
    navio/4,
    orientacao/1,
    tamanho_tabuleiro/1
]).


/* Determinando uma coordenada (X, Y) */
coordenada(X, Y) :-
    integer(X),
    integer(Y).


/* Definindo a estrutura do Navio
   navio(Tipo, Tamanho, Posicoes, PartesAtingidas) */
navio("Porta-Avioes", 5, [], []).
navio("Encouracado",  4, [], []).
navio("Submarino",    3, [], []).
navio("Cruzador",     3, [], []).
navio("Destroyer",    2, [], []).


/* Definindo a orientação do navio */
orientacao(h).
orientacao(v).


/* Definindo os "estados" de uma célula */
celula(agua).
celula(parte_navio).
celula(atingido).
celula(erro).


/* Exibição da célula (Celula, Simbolo) */
exibicao_celula(agua, "🌊").
exibicao_celula(parte_navio, "🚢").
exibicao_celula(atingido, "🔥").
exibicao_celula(erro, "❌").


/* Definindo o tamanho padrão do tabuleiro */
tamanho_tabuleiro(10).


/* Criação de um tabuleiro só com água (vazio) */
criacao_tabuleiro(Tabuleiro) :-
    tamanho_tabuleiro(N),
    length(Linha, N),
    maplist(=('🌊'), Linha),   % cada célula é "🌊"
    length(Tabuleiro, N),
    maplist(=(Linha), Tabuleiro).
