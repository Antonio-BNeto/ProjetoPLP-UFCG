:- module(controlador, [start/0]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(random)).

% Dependências do projeto
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1, criacao_tabuleiro/1, exibicao_celula/2, navios_disponiveis/1]).
:- use_module('../bot/bot', [gera_tabuleiro_bot/2, jogar/5]).
:- use_module('../logica/combate', [realizar_ataque/6, verifica_vitoria/1]).
:- use_module('../logica/posicionamento', [gera_navios/4]).
:- use_module('../jogo/tabuleiro', [tabuleiro_obter_celula/3]).

% Interface (arte / tela)
:- use_module('../interface/arte', [display_art/1, clear_screen/0]).

% --- Ponto de Entrada Principal ---
start :-
    menu_principal.

% --- Menu Principal ---
menu_principal :-
    clear_screen,
    display_art(menu),
    nl,
    writeln('1. Iniciar jogo'),
    writeln('2. Como Funciona'),
    writeln('3. Sair'),
    nl,
    write('Escolha uma opção: '), flush_output,
    read_line_to_string(user_input, Opcao),
    tratar_opcao(Opcao).

tratar_opcao("1") :-
    iniciar_jogo.
tratar_opcao("2") :-
    mostrar_como_funciona.
tratar_opcao("3") :-
    display_art(adeus),
    nl, writeln('Saindo...'), sleep(1),
    halt.
tratar_opcao(_) :-
    writeln("Opção inválida!"),
    sleep(1.5),
    menu_principal.

% --- Tela "Como Funciona" (Tópico 2) ---
mostrar_como_funciona :-
    clear_screen,
    display_art(jogo),
    nl,
    writeln('COMO FUNCIONA:'),
    nl,
    writeln('Bem-vindo ao clássico jogo de Batalha Naval!'),
    writeln('Sua missão: derrotar o inimigo afundando toda a frota dele.'),
    writeln('As embarcações são posicionadas automaticamente no início do jogo.'),
    writeln('No seu turno, digite uma coordenada no formato: linha coluna  (ex: 3 5)'),
    writeln('Entradas inválidas serão rejeitadas; não coloque ponto final.'),
    writeln('Após cada jogada, pressione Enter para prosseguir.'),
    nl,
    writeln('Pressione Enter para voltar ao menu...'),
    read_line_to_string(user_input, _),
    menu_principal.

% --- Preparação do Jogo ---
iniciar_jogo :-
    clear_screen,
    display_art(preparacao),
    % cria tabuleiro vazio e gera navios do jogador
    criacao_tabuleiro(TabVazio),
    navios_disponiveis(NaviosBase),
    gera_navios(NaviosBase, TabVazio, NaviosJogador, TabJogador),
    % gera tabuleiro e navios do bot
    gera_tabuleiro_bot(NaviosBotComPosicoes, TabBot),
    sleep(1.0),
    clear_screen,
    display_art(jogo),
    nl, writeln('Navios posicionados. Que a batalha comece!'),
    writeln('Pressione Enter para iniciar...'),
    read_line_to_string(user_input, _),
    loop_jogo(TabJogador, NaviosJogador, TabBot, NaviosBotComPosicoes, jogador).

% --- Loop Principal do Jogo ---
loop_jogo(TabJog, NavJog, TabBot, NavBot, Turno) :-
    clear_screen,
    display_art(jogo),
    exibir_tabuleiros(TabJog, TabBot),
    (   verifica_vitoria(NavBot) ->
            fim_de_jogo(vitoria, 'Parabéns! Você afundou toda a frota inimiga.')
    ;   verifica_vitoria(NavJog) ->
            fim_de_jogo(derrota, 'Sua frota foi destruída. Você perdeu.')
    ;   executar_turno(Turno, TabJog, NavJog, TabBot, NavBot, NovoTurno, T1, N1, T2, N2),
        loop_jogo(T1, N1, T2, N2, NovoTurno)
    ).

% --- Execução dos Turnos ---
executar_turno(jogador, TabJog, NavJog, TabBot, NavBot, bot, TabJog, NavJog, TabBotNovo, NavBotNovo) :-
    writeln('--- Seu turno ---'),
    repeat,
        obter_jogada_jogador(Coordenada),
        (   tabuleiro_obter_celula(TabBot, Coordenada, Celula)
        ->  (   member(Celula, [atingido, erro]) ->
                    writeln('** Você já atirou aí! Tente outra coordenada. **'),
                    fail
            ;   (   realizar_ataque(TabBot, NavBot, Coordenada, TabBotNovo, NavBotNovo, Resultado) ->
                        true
                ;   writeln('Erro ao realizar ataque.'), fail
                )
            )
        ;   writeln('** Coordenada inválida no tabuleiro alvo. Tente novamente. **'),
            fail
        ),
    !,  % sucesso no ataque
    exibir_resultado('Você', Resultado),
    writeln(''),
    writeln('Pressione Enter para continuar...'),
    read_line_to_string(user_input, _).

executar_turno(bot, TabJog, NavJog, TabBot, NavBot, jogador, TabJogNovo, NavJogNovo, TabBot, NavBot) :-
    writeln('--- Turno do Bot ---'),
    sleep(1.0),
    (   jogar(TabJog, NavJog, TabJogNovo, NavJogNovo, Resultado) ->
        true
    ;   % se algo falhar, mantém o estado e volta
        TabJogNovo = TabJog, NavJogNovo = NavJog, Resultado = erro
    ),
    exibir_resultado('O Bot', Resultado),
    sleep(1.2).

% --- Entrada Robusta do Jogador ---
obter_jogada_jogador(Coordenada) :-
    tamanho_tabuleiro(T),
    repeat,
        format('Digite a coordenada para atacar (Linha Coluna, ex: 3 5):~n> '), flush_output,
        read_line_to_string(user_input, LinhaStr),
        ( LinhaStr = "" -> writeln('** Entrada vazia. Digite dois números separados por espaço.'), fail
        ; true
        ),
        split_string(LinhaStr, " ", " \t\r\n", Partes),
        (   Partes = [XS, YS],
            catch(number_string(X, XS), _, fail),
            catch(number_string(Y, YS), _, fail)
        ->  (   integer(X), integer(Y),
                X >= 0, X < T, Y >= 0, Y < T
            ->  Coordenada = (X,Y), !
            ;   writeln('** Coordenada fora do tabuleiro. Tente novamente.'), fail
            )
        ;   writeln('** Entrada inválida. Digite dois números separados por espaço.'), fail
        ).

% --- Exibição de Resultados ---
exibir_resultado(_, coordenada_invalida) :-
    writeln('Coordenada inválida! (validação deve prevenir isso).').
exibir_resultado(Jogador, tiro_fora) :-
    format('~w acertou na água!~n', [Jogador]).
exibir_resultado(Jogador, acertou(Tipo)) :-
    format('~w acertou um navio (~w)!~n', [Jogador, Tipo]).
exibir_resultado(Jogador, afundou(Tipo)) :-
    format('~w AFUNDOU um navio (~w)!~n', [Jogador, Tipo]).
exibir_resultado(Jogador, acerto_repetido) :-
    format('~w atirou em um local já acertado.~n', [Jogador]).
exibir_resultado(Jogador, erro_repetido) :-
    format('~w atirou em um local que já havia errado.~n', [Jogador]).
exibir_resultado(_, Resultado) :-
    format('Resultado: ~w~n', [Resultado]).

% --- Exibição de Tabuleiros ---
exibir_tabuleiros(TabJog, TabBot) :-
    writeln('          SEU TABULEIRO                      TABULEIRO INIMIGO'),
    tamanho_tabuleiro(T),
    T1 is T - 1,
    % Cabeçalho eixo X
    write('   '), forall(between(0, T1, I), format('~d  ', [I])),
    write('     '),
    write('   '), forall(between(0, T1, I), format('~d  ', [I])),
    nl,
    % Linhas com eixo Y
    forall(between(0, T1, I),
           (   % coordenada Y do jogador
               format('~|~t~d~2+ ', [I]),
               nth0(I, TabJog, LinhaJog),
               maplist(exibicao_celula, LinhaJog, SimbolosJog),
               atomic_list_concat(SimbolosJog, ' ', LinhaJogStr),
               write(LinhaJogStr),
               write('      '),
               % coordenada Y do inimigo
               format('~|~t~d~2+ ', [I]),
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

% --- Fim de Jogo ---
fim_de_jogo(vitoria, Msg) :-
    clear_screen,
    display_art(vitoria),
    nl, writeln(Msg),
    writeln('Pressione Enter para voltar ao menu.'),
    read_line_to_string(user_input, _),
    menu_principal.

fim_de_jogo(derrota, Msg) :-
    clear_screen,
    display_art(derrota),
    nl, writeln(Msg),
    writeln('Pressione Enter para voltar ao menu.'),
    read_line_to_string(user_input, _),
    menu_principal.
