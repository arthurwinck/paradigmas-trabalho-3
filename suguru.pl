:- use_module(library(clpfd)).

matriz(
[
    [ponto(4,0),ponto(0,0),ponto(0,1),ponto(0,2),ponto(0,2),ponto(0,2)],
    [ponto(0,0),ponto(0,1),ponto(0,1),ponto(0,1),ponto(0,2),ponto(0,5)],
    [ponto(0,0),ponto(0,3),ponto(4,1),ponto(0,4),ponto(0,2),ponto(1,5)],
    [ponto(0,3),ponto(0,3),ponto(0,4),ponto(2,4),ponto(0,4),ponto(0,5)],
    [ponto(5,3),ponto(0,6),ponto(0,6),ponto(3,4),ponto(5,7),ponto(0,5)],
    [ponto(0,3),ponto(0,7),ponto(0,7),ponto(0,7),ponto(0,7),ponto(0,5)]
]
).


% Instancia um ponto com valor V e região R
criarPonto(V, R, ponto(V, R)) :- !.

% cria uma coordenada relacionada a um ponto com I e J
criarCoord(I, J, coord(I, J)) :- !.

% Recebe uma coordenada I,J e a matriz e retorna o ponto presente nessa coordenada
getP(coord(I, J), M, P) :-
    (
        length(M, Z1),
        I < Z1,
        I < 6,
    	I >= 0,
        nth0(0, M, L),
        length(L, Z2),
        J < Z2,
        J < 6,
        J >= 0,
        nth0(I, M, Li),
        nth0(J, Li, P),
        !
    );
    criarPonto(-1, -1, P),
    !.

% seta o elemento P no indíce K, esse é o método base
setaElemento([_|T], K, P, [P|T]) :-
    K == 0,
    !.

% Método recursivo de seta elemento
setaElemento([H|T], K, P, [H|L]) :-
    K1 is K - 1,
    setaElemento(T, K1, P, L),
    !.

% Seta um ponto na coordenada I, J da matriz M
setaP(coord(I, J), P, M, NM) :-
    nth0(I, M, L),
    setaElemento(L, J, P, NL),
    setaElemento(M, I, NL, NM),
    !.

% A partir de um ponto retorna o seu valor
getV(ponto(V, _), V).
% A partir de um ponto retorna a sua região
getR(ponto(_, R), R).

% A partir de uma região da coordenada C (I, J), retorna uma lista de coordenadas que possuem a mesma região
getRegiaoFromMatriz(C, M, Rs) :-
	getP(C, M, P),
    getR(P, R),
	findall(
        C1,
        (
        	nth0(0, M, L),
            length(M, MI1), length(L, MJ1),
            MI is MI1 - 1, MJ is MJ1 - 1,
            between(0, MI, I), between(0, MJ, J),
            criarCoord(I, J, C1), getP(C1, M, P1), getR(P1, R1), R1 == R
            ),
        Rs
    ).


% retorna uma lista de coordenadas vizinhas a coordenada repassada
getVizinhos(coord(I, J), Viz) :-
    criarCoord(I, J, C1),
    findall(
        C,
        (
            IH is I + 1, IL is I - 1, JH is J + 1, JL is J - 1,
            between(IL, IH, I1), between(JL, JH, J1),
            criarCoord(I1, J1, C), C \== C1
        ),
        Viz
    ).

% Retorna true caso o valor repassado não esteja presente na sua região e false se
% já estiver presente
verificaRegiao(C, N, M) :-
    getRegiaoFromMatriz(C, M, R),
    verificaLista(R, N, M),
    !.

% Retorna true se o valor não está presente nas adjacências da coordenada C e false caso estiver
verificaVizinhos(C, N, M) :-
    getVizinhos(C, Viz),
    verificaLista(Viz, N, M),
    !.

% Retorna true caso o valor não esteja presente na lista e vice-versa
verificaLista([], _, _).
verificaLista([H|T], N, M) :-
    getP(H, M, P),
    getV(P, V),
    V \== N,
    verificaLista(T, N, M),
    !.

% Realiza a verificação das lógicas do jogo: vizinhança e região. Checa se o valor está presente
% na vizinhança da coordenada em questão e na sua região. Um wrapper para as funções verificaVizinho
% e verificaRegiao
verifica(C, N, M) :-
    verificaRegiao(C, N, M),
    verificaVizinhos(C, N, M),
    !.

% Retorna a primeira coordenada C que está vazia (ponto(0,X)).
verificaVazio(M, C) :-
    (
        nth0(0, M, L),
        length(M, MI1),
        length(L, MJ1),
        MI is MI1 - 1,
        MJ is MJ1 - 1,
        between(0, MI, I),
        between(0, MJ, J),
        criarCoord(I, J, C),
        getP(C, M, P),
        getV(P, V),
        V == 0
    );
    criarCoord(-1, -1, C).

% Usando a função auxiliar verificar e a função findall, retorna uma lista de todos os possíveis
% valores para a coordenada C da matriz M
verificaPossiveis(C, M, S) :-
    getRegiaoFromMatriz(C, M, R),
    length(R, Max),
    findall(
        N,
        (
            between(1, Max, N),
            verifica(C, N, M)
        ),
        S
    ).

% Copia uma lista L
copiar(L, R) :- copiarAux(L,R).

% Auxiliar da função copiar
copiarAux([],R).
copiarAux([H|T], R) :- copiarAux(T,H).

% resolve o puzzle
%suguru(coord(I,J), M, _, M) :-
%    length(M, L), ((I >= L); (I < 0)), ((J >= L); (J < 0)), !.
suguru(coord(-1, -1), M, _, M) :- imprimeMatriz(M), !.
suguru(_, _, [], []).
suguru(C, M, [H|T], Rs) :-
    getP(C, M, P1),
    getR(P1, R1),
    criarPonto(H, R1, P),
    setaP(C, P, M, MN),
    verificaVazio(MN, C2),
    verificaPossiveis(C2, MN, S),
    suguru(C2, MN, S, R2),
    (
        length(R2, 0),
        suguru(C, M, T, _)
    );
    (
        length(R2, Q),
        Q \== 0,
        copiar(R2, Rs)
    ).

% entrada para solve
suguruHelper :-
    matriz(M),
    verificaVazio(M, C),
    verificaPossiveis(C, M, S),
    suguru(C, M, S, _),
    !.

% imprime o valor do ponto
imprimePonto(ponto(V, _)) :- write(V), tab(1).

% imprime uma linha da matriz
imprimeLinha([]) :- nl.
imprimeLinha([H|T]) :-
    imprimePonto(H),
    imprimeLinha(T),
    !.

% imprime a matriz
imprimeMatriz([]).
imprimeMatriz([H|T]) :- 
    imprimeLinha(H),
    imprimeMatriz(T),
    !.



oldsuguru(matriz) :-
    matriz(M),
    getVFromMatriz(M, V),
    append(V, Vs), Vs ins 1..6.
    
populaMatriz(M) :-
    append(M, Vs), Vs ins 1..6.

sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

suguruTeste(C, M, [H|_], ML) :-
    getP(C, M, P1),
    getR(P1, R),
    criarPonto(H, R, P),
    setaP(C, P, M, ML).
    %verificaVazio(ML, C2).
    %verificaPossiveis(C2, ML, S).

