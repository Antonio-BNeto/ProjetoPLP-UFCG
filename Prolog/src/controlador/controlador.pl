:- module(controlador, [start/0]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(system)).

% Dependências do projeto
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1, criacao_tabuleiro/1, exibicao_celula/2, navios_disponiveis/1]).
:- use_module('../bot/bot', [gera_tabuleiro_bot/2, jogar/5]).
:- use_module('../logica/combate', [realizar_ataque/6, verifica_vitoria/1]).
:- use_module('../logica/posicionamento', [gera_navios/4]).
:- use_module('../jogo/tabuleiro', [tabuleiro_obter_celula/3]).

% Interface
:- use_module('../interface/arte', [display_art/1, clear_screen/0]).

% --- Ponto de Entrada Principal ---
start :-
    menu_principal.

% --- Menu Principal ---
menu_principal :-
    arte:clear_screen,
    arte:display_art(menu),
    writeln('1. Iniciar jogo'),
    writeln('2. Como Funciona'),
    writeln('3. Sair'),
    writeln('Escolha uma opção: '),
    read_line_to_string(user_input, Opcao),
    tratar_opcao(Opcao).

tratar_opcao("1") :-
    iniciar_jogo.

tratar_opcao("2") :-
    arte:display_art(jogo), 
    writeln("Pressione Enter para voltar ao menu..."),
    read_line_to_string(user_input, _),
    menu_principal.
tratar_opcao("3") :-
    arte:display_art(adeus),
    halt.
tratar_opcao(_) :-
    writeln("Opção inválida!"),
    sleep(1.5),
    menu_principal.

% --- Preparação do Jogo ---
iniciar_jogo :-
    arte:clear_screen,
    arte:display_art(preparacao),
    criacao_tabuleiro(TabVazio),
    navios_disponiveis(NaviosBase),
    gera_navios(NaviosBase, TabVazio, NaviosJogador, TabJogador),
    gera_tabuleiro_bot(NaviosBotComPosicoes, TabBot),
    sleep(1.5),
    arte:display_art(jogo),
    loop_jogo(TabJogador, NaviosJogador, TabBot, NaviosBotComPosicoes, jogador).

% --- Loop Principal do Jogo ---
loop_jogo(TabJog, NavJog, TabBot, NavBot, Turno) :-
    arte:clear_screen,
    arte:display_art(jogo),        % <<--- mostra a arte do jogo no topo
    exibir_tabuleiros(TabJog, TabBot),

    (   verifica_vitoria(NavBot) ->
        arte:display_art(vitoria)
    ;   verifica_vitoria(NavJog) ->
        arte:display_art(derrota)
    ;   executar_turno(Turno, TabJog, NavJog, TabBot, NavBot, NovoTurno, T1, N1, T2, N2),
        loop_jogo(T1, N1, T2, N2, NovoTurno)
    ).

% --- Execução dos Turnos ---
executar_turno(jogador, TabJog, NavJog, TabBot, NavBot, bot, TabJog, NavJog, TabBotNovo, NavBotNovo) :-
    writeln('--- Seu turno ---'),
    repeat,
        obter_jogada_jogador(Coordenada),
        tabuleiro_obter_celula(TabBot, Coordenada, Celula),
        (   member(Celula, [atingido, erro])
        ->  writeln('** Você já atirou aí! Tente outra coordenada. **'),
            fail
        ;   realizar_ataque(TabBot, NavBot, Coordenada, TabBotNovo, NavBotNovo, Resultado),
            !
        )
    ,
    exibir_resultado('Você', Resultado),
    writeln('\nPressione Enter para continuar...'),
    read_line_to_string(user_input, _).

executar_turno(bot, TabJog, NavJog, TabBot, NavBot, jogador, TabJogNovo, NavJogNovo, TabBot, NavBot) :-
    writeln('--- Turno do Bot ---'),
    sleep(1),
    jogar(TabJog, NavJog, TabJogNovo, NavJogNovo, Resultado),
    exibir_resultado('O Bot', Resultado),
    sleep(2.5).

% --- Entrada Robusta do Jogador ---
obter_jogada_jogador(Coordenada) :-
    repeat,
        tamanho_tabuleiro(T),
        format('Digite a coordenada para atacar (Linha Coluna, ex: 3 5):~n> '),
        flush_output,
        read_line_to_string(user_input, Linha),
        split_string(Linha, " ", " \t\r\n", Partes),
        (   Partes = [XS, YS], number_string(X, XS), number_string(Y, YS)
        ->  (   X >= 0, X < T, Y >= 0, Y < T
            ->  Coordenada = (X,Y), !
            ;   writeln('** Coordenada fora do tabuleiro. Tente novamente.'), fail
            )
        ;   writeln('** Entrada inválida. Digite dois números separados por espaço.'), fail
        ).

% --- Exibição de Resultados e Tabuleiros ---
exibir_resultado(Jogador, tiro_fora) :- format('~w acertou na água!~n', [Jogador]).
exibir_resultado(Jogador, acertou(Tipo)) :- format('~w acertou um navio (~w)!~n', [Jogador, Tipo]).
exibir_resultado(Jogador, afundou(Tipo)) :- format('~w AFUNDOU um navio (~w)!~n', [Jogador, Tipo]).
exibir_resultado(Jogador, acerto_repetido) :- format('~w atirou em um local já acertado.~n', [Jogador]).
exibir_resultado(Jogador, erro_repetido) :- format('~w atirou em um local que já havia errado.~n', [Jogador]).
exibir_resultado(_, coordenada_invalida) :- writeln('Coordenada inválida! Isso não deveria acontecer com a validação de entrada.').

exibir_tabuleiros(TabJog, TabBot) :-
    writeln('          SEU TABULEIRO                      TABULEIRO INIMIGO'),
    tamanho_tabuleiro(T),
    T1 is T - 1,
    write('  '), forall(between(0, T1, I), format('~w  ', [I])),
    write('     '),
    write('  '), forall(between(0, T1, I), format('~w  ', [I])),
    nl,
    forall(between(0, T1, I),
           (   format('~|~` t~d~2+ ', [I]),
               nth0(I, TabJog, LinhaJog),
               maplist(exibicao_celula, LinhaJog, SimbolosJog),
               atomic_list_concat(SimbolosJog, ' ', LinhaJogStr),
               write(LinhaJogStr),
               write('    '),
               format('~|~` t~d~2+ ', [I]),
               nth0(I, TabBot, LinhaBot),
               maplist(ocultar_inimigo, LinhaBot, LinhaBotOculta),
               maplist(exibicao_celula, LinhaBotOculta, SimbolosBot),
               atomic_list_concat(SimbolosBot, ' ', LinhaBotStr),
               write(LinhaBotStr),
               nl
           )),
    nl.

% Helper para não mostrar os navios do inimigo
ocultar_inimigo(parte_navio, agua) :- !.
ocultar_inimigo(X, X).
