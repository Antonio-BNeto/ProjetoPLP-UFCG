:- module(jogo_arquitetura, [
    celula/1,
    criacao_tabuleiro/1,
    exibicao_celula/2,
    navios_disponiveis/1,
    tamanho_tabuleiro/1
]).

celula(agua).
celula(parte_navio).
celula(atingido).
celula(erro).

exibicao_celula(agua, '🌊').
exibicao_celula(parte_navio, '🚢').
exibicao_celula(atingido, '🔥').
exibicao_celula(erro, '❌').

tamanho_tabuleiro(10).

% Estrutura: navio(Tipo, Tamanho, Posicoes, PartesAtingidas)
navios_disponiveis([
    navio('Porta-Avioes', 5, [], []),
    navio('Encouracado',  4, [], []),
    navio('Submarino',    3, [], []),
    navio('Cruzador',     3, [], []),
    navio('Destroyer',    2, [], [])
]).

criacao_tabuleiro(Tabuleiro) :-
    tamanho_tabuleiro(N),
    length(Linha, N),
    maplist(=(agua), Linha), % CORRIGIDO: Usa 'agua' para consistência
    length(Tabuleiro, N),
    maplist(=(Linha), Tabuleiro).