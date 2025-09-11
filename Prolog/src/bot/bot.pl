:- module(bot_jogador, [
    gera_tabuleiro_bot/2,
    jogar/3
]).

:- use_module('../logica/posicionamento.pl').
:- use_module('../jogo/arquitetura.pl', [tamanho_tabuleiro/1]).

navios_do_bot([
    navio(pequeno,[]),
    navio(medio,[]),
    navio(grande,[])
]).

gera_tabuleiro_bot(NaviosPosicionados, TabuleiroFinal) :-
    navios_do_bot(Navios),
    gera_navios(Navios, [], NaviosPosicionados, [], TabuleiroFinal).

gera_navios([], Acc, Acc, Tab, Tab).
gera_navios([N|Ns], Acc, NaviosProntos, Tab, TabFinal) :-
    posicionar_navio(Tab, N, (NovoNavio, NovoTab)),
    gera_navios(Ns, [NovoNavio|Acc], NaviosProntos, NovoTab, TabFinal).

% --- Lógica do bot ---
jogar((Historico, exploracao, _), Coord, NovoEstado) :-
    tamanho_tabuleiro(T),
    Max is T-1,
    repeat,
    random_between(0, Max, X),
    random_between(0, Max, Y),
    Coord = (X,Y),
    \+ member(Coord, Historico), !,
    ( random_between(0,4,R), R =:= 0 ->
        NovoEstado = ([Coord|Historico], caca, Coord)
    ; NovoEstado = ([Coord|Historico], exploracao, _)
    ).

jogar((Historico, caca, Base), Coord, NovoEstado) :-
    vizinhos(Base, Vizinhos),
    exclude({Historico}/[C]>>member(C,Historico), Vizinhos, Livres),
    ( Livres = [Coord|_] ->
        ( random_between(0,2,R), R =:= 0 ->
            NovoEstado = ([Coord|Historico], caca, Coord)
        ;   NovoEstado = ([Coord|Historico], exploracao, _)
        )
    ; jogar((Historico, exploracao, _), Coord, NovoEstado)
    ).

vizinhos((X,Y), Vizinhos) :-
    X1 is X+1, X2 is X-1, Y1 is Y+1, Y2 is Y-1,
    findall((A,B),
        (member((A,B), [(X1,Y),(X2,Y),(X,Y1),(X,Y2)]),
         coordenada_valida((A,B))),
        Vizinhos).
