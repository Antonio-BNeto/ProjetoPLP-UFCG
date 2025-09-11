:- module(controlador, [start/0]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(random)).

% Dependências do projeto
:- use_module('../jogo/arquitetura', [tamanho_tabuleiro/1, exibicao_celula/2]).
:- use_module('../jogo/tabuleiro', [tabuleiro_obter_celula/3, tabuleiro_marcar_celula/4, tabuleiro_vazio/2]).
:- use_module('../bot/bot', [gera_tabuleiro_bot/2]).
:- use_module('../logica/combate', [realizar_ataque/6, verifica_vitoria/1]).
:- use_module('../interface/arte', [display_art/1, clear_screen/0]).
:- use_module('../logica/posicionamento', [
    posicionar_navio/3
]).

% Delays configuráveis (segundos)
delay_menu(1.0).
delay_loading(0.4).
delay_game(0.7).

% -------------------------------------------------------------
% Ponto de entrada
% -------------------------------------------------------------
start :-
    clear_screen,
    mostrar_tela_inicial.

% -------------------------------------------------------------
% Tela inicial / menu
% -------------------------------------------------------------
mostrar_tela_inicial :-
    clear_screen,
    display_art(menu),
    nl,
    writeln('                                      Escolha uma opcao:'),
    writeln('                                      [1] Novo Jogo'),
    writeln('                                      [2] Sair'),
    writeln('                                      [3] Como Funciona'),
    write('                                      > '), flush_output,
    read_line_to_codes(user_input, Codes),
    string_codes(Str, Codes),
    ( Str = "1" -> start_game
    ; Str = "2" -> ( display_art(adeus), delay_for_menu, ! )
    ; Str = "3" -> ( mostrar_descricao, delay_for_menu, mostrar_tela_inicial )
    ;  writeln('Opcao invalida!'), delay_for_menu, mostrar_tela_inicial
    ).

delay_for_menu :- delay_menu(T), sleep(T).

mostrar_descricao :-
    clear_screen,
    display_art(jogo),
    nl,
    writeln('COMO FUNCIONA:'), nl,
    writeln('Bem-vindo ao classico jogo de Batalha Naval!'),
    writeln('Sua missao: derrotar o inimigo afundando toda a frota dele.'), nl,
    writeln('As embarcacoes sao posicionadas automaticamente.'), nl,
    writeln('Pressione Enter para voltar.'),
    read_line_to_codes(user_input, _).

% -------------------------------------------------------------
% Iniciar jogo: posiciona navios automaticamente
% -------------------------------------------------------------
start_game :-
    clear_screen,
    writeln('Posicionando seus navios...'),
    criar_frota(TabJog, NavJog),
    delay_loading,
    writeln('Posicionando os navios do bot...'),
    criar_frota(TabBot, NavBot),
    delay_loading,
    writeln('Preparando campo de batalha...'), nl,
    writeln('Navios posicionados. Que a batalha comece!'),
    writeln('\nPressione Enter para iniciar...'),
    read_line_to_codes(user_input, _),
    loop_jogo(TabJog, NavJog, TabBot, NavBot, [], []).

% -------------------------------------------------------------
% Cria frota completa (tabuleiro + lista de navios)
% -------------------------------------------------------------
criar_frota(Tab, ListaNavios) :-
    % inicializa tabuleiro vazio
    tamanho_tabuleiro(T),
    tabuleiro_vazio(T, Tab),
    % lista de tipos de navios
    NaviosTipos = [porta_avioes, encouracado, cruzador, destroyer, submarino],
    % posiciona cada navio
    criar_navios(Tab, NaviosTipos, ListaNavios).

criar_navios(_, [], []).
criar_navios(Tab, [Tipo|RestoTipos], [NavioAtualizado|RestoNavios]) :-
    % cria navio vazio
    Navio = navio(Tipo, [], []),
    % posiciona no tabuleiro
    posicionar_navio_tab(Tab, Navio, TabAtualizado),
    NavioAtualizado = Navio,
    % continua com o restante da frota
    criar_navios(TabAtualizado, RestoTipos, RestoNavios).

% -------------------------------------------------------------
% Loop principal do jogo
% -------------------------------------------------------------
loop_jogo(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot) :-
    clear_screen,
    mostrar_tabuleiros_lado_a_lado(TabJog, TabBot),
    nl, writeln('Sua vez de atirar! (formato: linha coluna, ex: 3 5)'),
    ler_coordenada(CoordJog),
    ( realizar_ataque(TabBot, NavBot, CoordJog, TabBotAtualizado, NavBotAtualizado, ResJog) -> true
    ;  writeln('Erro ao realizar ataque.'), delay_game, loop_jogo(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot)
    ),
    clear_screen,
    mostrar_tabuleiros_lado_a_lado(TabJog, TabBotAtualizado),
    mostrar_resultado('Voce', ResJog),
    ( ResJog = acerto_repetido ; ResJog = erro_repetido ->
        delay_game, loop_jogo(TabJog, NavJog, TabBotAtualizado, NavBotAtualizado, TirosJog, TirosBot)
    ; ( verifica_vitoria(NavBotAtualizado) ->
            fim_de_jogo(vitoria, 'Parabens! Afundou toda a frota inimiga.')
      ;
            turno_do_bot(TabJog, NavJog, TabBotAtualizado, NavBotAtualizado, [CoordJog|TirosJog], TirosBot)
      )
    ).

delay_game :- delay_game(T), sleep(T).

% -------------------------------------------------------------
% Turno do bot
% -------------------------------------------------------------
turno_do_bot(TabJog, NavJog, TabBot, NavBot, TirosJog, TirosBot) :-
    delay_loading,
    writeln('Turno do inimigo...'),
    tamanho_tabuleiro(T),
    Max is T - 1,
    repeat,
      random_between(0, Max, X),
      random_between(0, Max, Y),
      CoordBot = (X,Y),
      \+ member(CoordBot, TirosBot),
    !,
    ( realizar_ataque(TabJog, NavJog, CoordBot, TabJogAtualizado, NavJogAtualizado, ResBot) -> true ; (
        writeln('Erro no ataque do bot.'), fail)
    ),
    clear_screen,
    mostrar_tabuleiros_lado_a_lado(TabJogAtualizado, TabBot),
    format('Inimigo atirou em: ~w~n', [CoordBot]),
    mostrar_resultado('Inimigo', ResBot),
    ( ResBot = acerto_repetido ; ResBot = erro_repetido ->
        delay_game, loop_jogo(TabJogAtualizado, NavJogAtualizado, TabBot, NavBot, TirosJog, [CoordBot|TirosBot])
    ; ( verifica_vitoria(NavJogAtualizado) ->
            fim_de_jogo(derrota, 'Sua frota foi destruida. Voce perdeu.')
      ;
            delay_game, loop_jogo(TabJogAtualizado, NavJogAtualizado, TabBot, NavBot, TirosJog, [CoordBot|TirosBot])
      )
    ).

% -------------------------------------------------------------
% Funções auxiliares de impressão do tabuleiro
% -------------------------------------------------------------
mostrar_tabuleiros_lado_a_lado(Tab1, Tab2) :-
    tamanho_tabuleiro(N),
    write('   '), imprimir_indices(N), write('      '), write('   '), imprimir_indices(N), nl,
    mostrar_linhas(0, N, Tab1, Tab2).

imprimir_indices(N) :-
    MaxIndex is N - 1,
    forall(between(0, MaxIndex, I), ( format('~|~`0t~d~2+ ', [I]) ) ).

mostrar_linhas(I, N, _, _) :- I >= N, !.
mostrar_linhas(I, N, Tab1, Tab2) :-
    findall(CellStr1, (between(0, N-1, J), cell_display(Tab1, (I,J), false, CellStr1)), Row1List),
    findall(CellStr2, (between(0, N-1, J), cell_display(Tab2, (I,J), true,  CellStr2)), Row2List),
    atomic_list_concat(Row1List, ' ', Row1),
    atomic_list_concat(Row2List, ' ', Row2),
    format('~|~`0t~d~2+  ~w      ~|~`0t~d~2+  ~w~n', [I, Row1, I, Row2]),
    I1 is I + 1,
    mostrar_linhas(I1, N, Tab1, Tab2).

cell_display(Tab, (X,Y), Hide, Str) :-
    ( tabuleiro_obter_celula(Tab, (X,Y), Val) ->
        display_for(Val, Hide, Str)
    ; exibicao_celula(agua, Str)
    ).

display_for(parte_navio, true, Str) :- !, exibicao_celula(agua, Str).
display_for(Val, _, Str) :- exibicao_celula(Val, Str).

% -------------------------------------------------------------
% Mensagens de resultado
% -------------------------------------------------------------
mostrar_resultado(_Actor, coordenada_invalida) :- writeln('Coordenada invalida. Tente novamente.'), !.
mostrar_resultado(Actor, tiro_fora) :- format('~w: Errou.~n', [Actor]), !.
mostrar_resultado(Actor, acertou(_Tipo)) :- format('~w: Acertou!~n', [Actor]), !.
mostrar_resultado(Actor, afundou(_Tipo)) :- format('~w: Afundou um navio!~n', [Actor]), !.
mostrar_resultado(_, acerto_repetido) :- writeln('Ja atirou nesta posicao.'), !.
mostrar_resultado(_, erro_repetido)  :- writeln('Ja atirou nesta posicao.'), !.
mostrar_resultado(_, Resultado) :- format('Resultado: ~w~n', [Resultado]).

% -------------------------------------------------------------
% Fim de jogo
% -------------------------------------------------------------
fim_de_jogo(vitoria, Msg) :-
    clear_screen,
    display_art(vitoria),
    nl, writeln(Msg),
    writeln('\nPressione Enter para voltar ao menu.'),
    read_line_to_codes(user_input, _),
    mostrar_tela_inicial.

fim_de_jogo(derrota, Msg) :-
    clear_screen,
    display_art(derrota),
    nl, writeln(Msg),
    writeln('\nPressione Enter para sair').
