:- use_module('bot/Bot.pl').
:- use_module('Logica/Posicionamento.pl', [
    coordenada_valida/1,
    tamanho_tabuleiro/1
]).

:- begin_tests(bot_jogador).

test(gera_tabuleiro_bot) :-
    gera_tabuleiro_bot(Navios, _Tab),  % Tab não usado, evita warning
    length(Navios, 3).

test(jogar_exploracao) :-
    jogar(([], exploracao, _), C, (H,_,_)),
    assertion(member(C,H)).

test(jogar_caca) :-
    jogar(([(0,0)], caca, (0,0)), C, (H,Modo,_)),
    assertion(member(C,H)),
    member(Modo, [caca, exploracao]).

:- end_tests(bot_jogador).

% Atalho para rodar todos os testes do bot
run_tests_all :-
    run_tests([bot_jogador]).
