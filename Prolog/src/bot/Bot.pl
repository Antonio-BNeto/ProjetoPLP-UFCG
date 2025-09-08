:- module(bot_jogador, [
    gera_tabuleiro_bot/2,
    jogar/3,
    run_tests/0
]).

:- use_module(logica_posicionamento).

% ============================================
% NAVIOS DO BOT
% ============================================

navios_do_bot([
    navio(pequeno,[]),
    navio(medio,[]),
    navio(grande,[])
]).

% ============================================
% TABULEIRO INICIAL
% ============================================

gera_tabuleiro_bot(NaviosPosicionados, TabuleiroFinal) :-
    navios_do_bot(Navios),
    gera_navios(Navios, [], NaviosPosicionados, [], TabuleiroFinal).

gera_navios([], Acc, Acc, Tab, Tab).
gera_navios([N|Ns], Acc, NaviosProntos, Tab, TabFinal) :-
    posicionar_navio(Tab, N, (NovoNavio, NovoTab)),
    gera_navios(Ns, [NovoNavio|Acc], NaviosProntos, NovoTab, TabFinal).

% ============================================
% LÓGICA DE JOGADA DO BOT
% ============================================

% Estado do bot: (Historico, Modo, Alvo)
% Historico = lista de coordenadas já jogadas
% Modo = exploracao | caca (quando já acertou e tenta expandir)
% Alvo = coordenada base do último acerto

% jogar(+Estado, -Coord, -NovoEstado)
jogar((Historico, exploracao, _), Coord, NovoEstado) :-
    % 1. Gera coordenada aleatória que ainda não foi usada
    tamanho_tabuleiro(T),
    Max is T-1,
    repeat,
    random_between(0, Max, X),
    random_between(0, Max, Y),
    Coord = (X,Y),
    \+ member(Coord, Historico), !,
    % decide se foi "acerto" ou não (mock, só pra lógica de IA)
    ( random_between(0,4,R), R =:= 0 -> % 20% chance de "acerto"
        NovoEstado = ([Coord|Historico], caca, Coord)
    ;   NovoEstado = ([Coord|Historico], exploracao, _)
    ).

jogar((Historico, caca, Base), Coord, NovoEstado) :-
    % 2. Se em modo caça, tenta vizinhos do último acerto
    vizinhos(Base, Vizinhos),
    exclude({Historico}/[C]>>member(C,Historico), Vizinhos, Livres),
    ( Livres = [Coord|_] ->
        % ainda tem vizinhos livres
        ( random_between(0,2,R), R =:= 0 -> % 33% chance de "novo acerto"
            NovoEstado = ([Coord|Historico], caca, Coord)
        ;   NovoEstado = ([Coord|Historico], exploracao, _)
        )
    ; % se não há vizinhos livres, volta a explorar
        jogar((Historico, exploracao, _), Coord, NovoEstado)
    ).

% vizinhos((X,Y), Lista)
vizinhos((X,Y), Vizinhos) :-
    X1 is X+1, X2 is X-1, Y1 is Y+1, Y2 is Y-1,
    findall((A,B),
        (member((A,B), [(X1,Y),(X2,Y),(X,Y1),(X,Y2)]),
         coordenada_valida((A,B))),
        Vizinhos).

% ============================================
% TESTES
% ============================================

:- begin_tests(bot_jogador).

test(gera_tabuleiro_bot) :-
    once((
        gera_tabuleiro_bot(Navios, Tab),
        length(Navios, 3),
        forall(member(navio(_,Pos),Navios),
               forall(member(C,Pos), obter(Tab,C,parte_navio)))
    )).

test(jogar_exploracao) :-
    jogar(([], exploracao, _), C, (H,_,_)),
    assertion(member(C,H)).

test(jogar_caca_ou_retorna_exploracao) :-
    jogar(([(0,0)], caca, (0,0)), C, (H,Modo,_)),
    assertion(member(C,H)),
    member(Modo, [caca, exploracao]).

:- end_tests(bot_jogador).

% atalho para rodar esta suíte
run_all_tests :- run_tests([bot_jogador]).
