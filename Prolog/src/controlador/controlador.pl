:- module(controlador, [start/0]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(system)).

% Dependências do projeto 
:- use_module('../jogo/arquitetura').
:- use_module('../bot/bot').
:- use_module('../logica/combate').

% Delays (segundos)
delay_menu(1.5).
delay_loading(0.5).
delay_game(1.0).

% Ponto de entrada
start :-
    clear_screen,
    mostrar_tela_inicial.

% Menu principal
mostrar_tela_inicial :-
    clear_screen,
    display_art(menu),
    nl,
    writeln('                                      Escolha uma opcaO:'),
    writeln('                                      [1] Novo Jogo'),
    writeln('                                      [2] Sair'),
    writeln('                                      [3] Como Funciona'),
    write('                                      > '), flush_output,
    read_line_to_codes(user_input, Codes),
    string_codes(Str, Codes),
    ( Str = "1" -> start_game
    ; Str = "2" -> display_art(adeus), delay_for_menu
    ; Str = "3" -> mostrar_descricao
    ; writeln('OpcaO invalida!'), delay_for_menu, mostrar_tela_inicial
    ).

delay_for_menu :- delay_menu(T), sleep(T).

% Descricao do jogo
mostrar_descricao :-
    clear_screen,
    display_art(jogo),
    nl,
    writeln('COMO FUNCIONA:'), nl,
    writeln('Bem-vindo ao classico jogo de Batalha Naval!'),
    writeln('Sua missao: derrotar o inimigo afundando toda a frota dele.'), nl,
    writeln('OBJETIVO:'),
    writeln('  ⚓ Afundar todas as embarcacOes inimigas antes que a sua frota seja destruida.'), nl,
    writeln('POSICIONAMENTO DOS NAVIOS:'),
    writeln('  - As embarcacOes sao posicionadas AUTOMATICAMENTE.'),
    writeln('  - Voce nao precisa se preocupar em posiciona-las manualmente.'), nl,
    writeln('FROTA DISPONIVEL:'),
    writeln('  - Porta-Avioes (5 espacos)'),
    writeln('  - Encouracado (4 espacos)'),
    writeln('  - Submarino (3 espacos)'),
    writeln('  - Cruzador (3 espacos)'),
    writeln('  - Destroyer (2 espacos)'), nl,
    writeln('COMO ATACAR:'),
    writeln('  - O tabuleiro possui linhas e colunas numeradas.'),
    writeln('  - Para atacar, digite as coordenadas no formato:'),
    writeln('        linha coluna'),
    writeln("    (Exemplo: '3 5' -> significa linha 3, coluna 5.)"), nl,
    writeln('FEEDBACK DO ATAQUE:'),
    writeln('  🔥  : Acerto em uma embarcacAo'),
    writeln('  ❌  : Tiro na agua'),
    writeln('  🚢  : Parte de navio'),
    writeln('  🌊  : Água'), nl,
    writeln('DICA DE ESTRATEGIA:'),
    writeln('  Use a logica: apos um acerto, ataque nas casas vizinhas para'),
    writeln('  aumentar as chances de afundar o navio.'), nl,
    writeln('Pressione Enter para voltar ao menu principal...'),
    read_line_to_codes(user_input, _),
    mostrar_tela_inicial.

% Inicia jogo: posiciona navios (automaticamente) e entra no loop
start_game :-
    clear_screen,
    writeln('Posicionando seus navios...'),
    escolher_navios_bot(NavJog, TabJog),
    delay_loading,
    writeln('Posicionando os navios do bot...'),
    escolher_navios_bot(NavBot, TabBot),
    delay_loading,
    writeln('Preparando campo de batalha...'),
    delay_loading,
    writeln('Navios posicionados. Que a batalha comece!'),
    writeln('\nPressione Enter para iniciar...'),
    read_line_to_codes(user_input, _),
    loop_jogo(TabJog, NavJog, TabBot, NavBot, [], []).

delay_loading :- delay_loading(T), sleep(T).

% Loop principal
loop_jogo(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot) :-
    clear_screen,
    maplist(maplist(ocultar), TabBot, TabBotVisivel),
    display_art(jogo),
    mostrar_tabuleiros_lado_a_lado(TabJog, TabBotVisivel),
    nl, writeln('Sua vez de atirar!'),
    ler_coordenada(CoordJog),
    ( realizar_ataque(TabBot, NavBot, CoordJog, TabBotAtualizado, NavBotAtualizado, Res) -> true ; (
          writeln('Erro ao realizar ataque.'), fail)
    ),
    clear_screen,
    display_art(jogo),
    maplist(maplist(ocultar), TabBotAtualizado, TabBotOcultado),
    mostrar_tabuleiros_lado_a_lado(TabJog, TabBotOcultado),
    mostrar_resultado('Voce', Res),
    ( Res = acerto_repetido ; Res = erro_repetido ->
        % jogada invalida: pausa e repete
        delay_game, loop_jogo(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot)
    ;
        ( verifica_vitoria(NavBotAtualizado) ->
            fim_de_jogo(vitoria, 'Parabens! Afundou todos os navios inimigos e venceu!')
        ;
            turno_do_bot(TabJog, NavJog, TabBotAtualizado, NavBotAtualizado, [CoordJog|TirosJog], TirosBot)
        )
    ).

delay_game :- delay_game(T), sleep(T).

% Turno do bot
turno_do_bot(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot) :-
    delay_loading,
    nl, writeln('Turno do inimigo.'),
    writeln('Pressione Enter para continuar...'), read_line_to_codes(user_input, _),
    realizar_jogada_bot(TirosBot, CoordBot),
    ( realizar_ataque(TabJog, NavJog, CoordBot, TabJogAtualizado, NavJogAtualizado, ResBot) -> true ; (
          writeln('Erro no ataque do bot.'), fail)
    ),
    clear_screen,
    display_art(jogo),
    maplist(maplist(ocultar), TabBot, TabBotOculto),
    mostrar_tabuleiros_lado_a_lado(TabJogAtualizado, TabBotOculto),
    format('\nO inimigo atacou em: ~w~n', [CoordBot]),
    delay_loading,
    mostrar_resultado('O inimigo', ResBot),
    delay_loading,
    ( verifica_vitoria(NavJogAtualizado) ->
        fim_de_jogo(derrota, 'O inimigo afundou todos os seus navios! Voce perdeu.')
    ;
        writeln('\nPressione Enter para continuar...'), read_line_to_codes(user_input, _),
        loop_jogo(TabJogAtualizado, NavJogAtualizado, TabBot, NavBot, TirosJog, [CoordBot|TirosBot])
    ).

% Fim de jogo
fim_de_jogo(Arte, Mensagem) :-
    clear_screen,
    display_art(Arte),
    format('\n~w~n', [Mensagem]),
    writeln('\nPressione Enter para voltar ao menu...'), read_line_to_codes(user_input, _),
    mostrar_tela_inicial.

% Funcoes auxiliares
% ocultar: se ParteNavio -> Agua, caso contrario mantem
ocultar(parte_navio, agua) :- !.
ocultar(X, X).

% mostrar tabuleiros lado a lado
mostrar_tabuleiros_lado_a_lado(Tab1, Tab2) :-
    % obtem tamanho do tabuleiro consultando o modulo de arquitetura
    ( tamanho_tabuleiro(N) -> true ; N = 8 ),
    BoardWidth = 34,
    Gap = '     ',
    Title1 = 'Seu Tabuleiro', Title2 = 'Tabuleiro do Inimigo',
    center(BoardWidth, Title1, C1), center(BoardWidth, Title2, C2),
    format('~n~s~s~s~n', [C1, Gap, C2]),
    % cabecalho
    format('    ', []), forall(between(0, N1, I), (N1 is N-1, format('~|~`0t~d~2+ ', [I]))), format('~s~n', [Gap]),
    % separador simples
    length_sep(BoardWidth, Sep), format('~s~s~n', [Sep, Gap]),
    % imprimir linhas
    print_rows(0, Tab1, Tab2, Gap).

center(Width, Str, Out) :-
    string_length(Str, Len),
    Padding is Width - Len,
    Left is Padding // 2,
    Right is Padding - Left,
    string_chars(LeftS, Left), string_chars(RightS, Right),
    string_concat(LeftS, Str, Temp), string_concat(Temp, RightS, Out).

% gera string com n repeticoes de espaco (util)
string_chars(S, N) :- N > 0, string_chars(S, _), !, fail.
string_chars(S, 0) :- S = "".
string_chars(S, N) :- N > 0, N1 is N-1, string_chars(S1, N1), string_concat(' ', S1, S).

length_sep(N, Sep) :- length_sep_acc(N, '', Sep).
length_sep_acc(0, Acc, Acc).
length_sep_acc(N, Acc, Sep) :- N>0, string_concat('-', Acc, Acc2), N1 is N-1, length_sep_acc(N1, Acc2, Sep).

print_rows(_, [], [], _) :- !.
print_rows(I, [L1|Ls1], [L2|Ls2], Gap) :-
    % converte cada linha para string usando exibicao_celula/1 do modulo arquitectura
    maplist(exibicao_celula, L1, StrCells1), atomic_list_concat(StrCells1, ' ', Line1),
    maplist(exibicao_celula, L2, StrCells2), atomic_list_concat(StrCells2, ' ', Line2),
    format('~|~`0t~d~2+ | ~s~s~s~n', [I, Line1, Gap, Line2]),
    I1 is I+1,
    print_rows(I1, Ls1, Ls2, Gap).

% mostrar resultado
mostrar_resultado(Jogador, Resultado) :-
    resultado_msg(Resultado, Msg), format('\n~w ~w~n', [Jogador, Msg]).

resultado_msg(acertou(N), Msg) :- format(string(Msg), 'acertou o navio ~w!', [N]).
resultado_msg(afundou(N), Msg) :- format(string(Msg), 'AFUNDOU o navio ~w!', [N]).
resultado_msg(tiro_fora, 'errou.').
resultado_msg(acerto_repetido, 'ja atirou nesta posicao.').
resultado_msg(erro_repetido, 'ja atirou nesta posicao.').
resultado_msg(_, 'resultado desconhecido.').

% ler coordenada: espera 'linha coluna' como entrada
ler_coordenada((X,Y)) :-
    write('Digite a coordenada para atacar (ex: 3 5): '), flush_output,
    read_line_to_codes(user_input, Codes), string_codes(Str, Codes),
    split_string(Str, " ", "\s\t\n", Parts),
    ( Parts = [SX, SY], catch(number_string(X, SX), _, fail), catch(number_string(Y, SY), _, fail) ->
        ( tamanho_tabuleiro(T), X >= 0, X < T, Y >= 0, Y < T -> true ; (writeln('Coordenada fora do tabuleiro. Tente novamente.'), ler_coordenada((X,Y))) )
    ; writeln('Entrada invalida. Use dois numeros separados por espaco.'), ler_coordenada((X,Y))
    ).