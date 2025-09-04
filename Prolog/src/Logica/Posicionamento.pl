:- module(logica_posicionamento, [
    gera_corpo_embarcacao/4,
    valida_posicionamento/4,
    marca_navios_no_tabuleiro/3,
    posicionar_navio/3,
    run_tests/0
]).

% ============================================
% DEFINIÇÕES AUXILIARES (mock simplificado)
% ============================================

% Tamanho do tabuleiro
tamanho_tabuleiro(10).

% Estrutura de navio (mock)
% tamanho/2: retorna o tamanho do navio
tamanho(navio(pequeno,_), 2).
tamanho(navio(medio,_), 3).
tamanho(navio(grande,_), 4).

% set_posicoes/3: atualiza o navio com posições
set_posicoes(navio(Tipo,_), Pos, navio(Tipo,Pos)).

% Células possíveis do tabuleiro
% agua = célula vazia
% parte_navio = parte de navio
% atingido, erro = estados depois de ataques
celula(agua).
celula(parte_navio).
celula(atingido).
celula(erro).

% Tabuleiro representado como lista de pares (Coord,Valor)
% obter/3: pega valor da célula
obter(Tab, Coord, Valor) :-
    ( member((Coord,V), Tab) -> Valor = V ; Valor = agua ).

% marca/4: atualiza célula no tabuleiro
marca(Tab, Coord, Valor, NovoTab) :-
    select((Coord,_), Tab, Restante) -> NovoTab = [(Coord,Valor)|Restante]
    ; NovoTab = [(Coord,Valor)|Tab].

% gera_coordenada/1: coordenada aleatória dentro do tabuleiro
gera_coordenada((X,Y)) :-
    tamanho_tabuleiro(T),
    Max is T-1,
    random_between(0, Max, X),
    random_between(0, Max, Y).

% gera_orientacao/1: orientação aleatória
gera_orientacao(h) :-
    random_between(0, 1, V),
    V =:= 0, !.
gera_orientacao(v).

% ============================================
% REGRAS PRINCIPAIS
% ============================================

% Verifica se coordenada está dentro do tabuleiro
coordenada_valida((X,Y)) :-
    tamanho_tabuleiro(T),
    X >= 0, Y >= 0,
    X < T, Y < T.

% Gera corpo do navio
gera_corpo_embarcacao((X,Y), Navio, h, Corpo) :-
    tamanho(Navio, Tam),
    T1 is Tam-1,
    findall((X1,Y),
        (between(0, T1, I), X1 is X+I),
        Corpo).

gera_corpo_embarcacao((X,Y), Navio, v, Corpo) :-
    tamanho(Navio, Tam),
    T1 is Tam-1,
    findall((X,Y1),
        (between(0, T1, I), Y1 is Y+I),
        Corpo).

% Valida se pode posicionar navio
valida_posicionamento(Tab, Coord, Navio, Orient) :-
    gera_corpo_embarcacao(Coord, Navio, Orient, Corpo),
    maplist(coordenada_valida, Corpo),
    maplist(celula_livre(Tab), Corpo).

celula_livre(Tab, Coord) :- obter(Tab, Coord, agua).

% Marca navios no tabuleiro
marca_navios_no_tabuleiro(Tab, [], Tab).
marca_navios_no_tabuleiro(Tab, [C|Cs], NovoTab) :-
    marca(Tab, C, parte_navio, Tab1),
    marca_navios_no_tabuleiro(Tab1, Cs, NovoTab).

% Posiciona navio até sucesso
posicionar_navio(Tab, Navio, (NovoNavio, NovoTab)) :-
    gera_coordenada(Pos),
    gera_orientacao(Orient),
    ( valida_posicionamento(Tab, Pos, Navio, Orient) ->
        gera_corpo_embarcacao(Pos, Navio, Orient, Corpo),
        marca_navios_no_tabuleiro(Tab, Corpo, NovoTab),
        set_posicoes(Navio, Corpo, NovoNavio)
    ; posicionar_navio(Tab, Navio, (NovoNavio, NovoTab))
    ).

% ============================================
% TESTES
% ============================================

:- begin_tests(logica_posicionamento).

test(coordenada_valida_ok) :-
    coordenada_valida((0,0)),
    coordenada_valida((9,9)).

test(coordenada_valida_fail, [fail]) :-
    coordenada_valida((10,0)).

test(gera_corpo_horiz) :-
    gera_corpo_embarcacao((2,5), navio(medio,[]), h, Corpo),
    assertion(Corpo == [(2,5),(3,5),(4,5)]).

test(gera_corpo_vert) :-
    gera_corpo_embarcacao((7,1), navio(pequeno,[]), v, Corpo),
    assertion(Corpo == [(7,1),(7,2)]).

test(valida_posicionamento_inicial) :-
    once(valida_posicionamento([], (0,0), navio(pequeno,[]), h)).

test(marca_navio) :-
    once((
        marca_navios_no_tabuleiro([], [(0,0),(0,1)], NovoTab),
        obter(NovoTab, (0,0), parte_navio),
        obter(NovoTab, (0,1), parte_navio)
    )).

test(posicionar_navio) :-
    once((
        posicionar_navio([], navio(pequeno,[]), (NavioNovo, Tab)),
        NavioNovo = navio(_,Posicoes),
        length(Posicoes, 2),
        forall(member(C,Posicoes), obter(Tab,C,parte_navio))
    )).

:- end_tests(logica_posicionamento).

% Atalho para rodar apenas esta suíte de testes
run_all_tests :- run_tests([logica_posicionamento]).

