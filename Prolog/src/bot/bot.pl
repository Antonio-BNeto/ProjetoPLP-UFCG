:- module(bot, [
    gera_tabuleiro_bot/2,
    jogar/5
]).

:- use_module('../logica/posicionamento', [gera_navios/4]).
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1, criacao_tabuleiro/1, navios_disponiveis/1]).
:- use_module('../logica/combate', [realizar_ataque/6]).


gera_tabuleiro_bot(NaviosPosicionados, TabuleiroFinal) :-
    criacao_tabuleiro(TabuleiroVazio),
    navios_disponiveis(Navios), 
    gera_navios(Navios, TabuleiroVazio, NaviosPosicionados, TabuleiroFinal).


jogar(TabJogIn, NavJogIn, TabJogOut, NavJogOut, Resultado) :-
    tamanho_tabuleiro(T),
    T1 is T - 1,
    repeat,
        random_between(0, T1, X),
        random_between(0, T1, Y),
        Coordenada = (X,Y),
        realizar_ataque(TabJogIn, NavJogIn, Coordenada, TabJogOut, NavJogOut, Resultado),
        \+ member(Resultado, [acerto_repetido, erro_repetido]),
    !.