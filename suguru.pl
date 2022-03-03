:- use_module(library(clpfd)).

%! Instanciação da matriz
matriz(
[
    [ponto(4,0),ponto(0,0),ponto(0,1),ponto(0,2),ponto(0,2),ponto(0,2)],
    [ponto(0,0),ponto(0,1),ponto(0,1),ponto(0,1),ponto(0,2),ponto(0,5)],
    [ponto(0,4),ponto(0,3),ponto(4,1),ponto(0,4),ponto(0,2),ponto(1,5)],
    [ponto(0,3),ponto(0,3),ponto(0,4),ponto(2,4),ponto(0,4),ponto(0,5)],
    [ponto(5,3),ponto(0,6),ponto(0,6),ponto(3,4),ponto(5,7),ponto(0,5)],
    [ponto(0,3),ponto(0,7),ponto(0,7),ponto(0,7),ponto(0,7),ponto(0,5)]
]
).
%! Seta o elemento

%! retorna um ponto a partir de um valor e uma região
criarPonto(V, R, ponto(V, R)) :- !.

%! retorna uma coordenada a partir de i e j especificados
criarCoord(I, J, coord(I,J)) :- !.

%! Retorna a região do ponto específico
getR(ponto(_,R), R).

%! Retorna o valor do ponto específico
getV(ponto(V,_), V).

%! retorna um ponto (valor e região) / Testa os limites da matriz. Caso não satisfaça a condição,
%! retorna um ponto inválido
getP(coord(I,J), M, P) :-
    (
    length(M, L1), I >= 0, L1 > I, nth0(0, M, L),
    length(L, L2), J >= 0, L2 > J, nth0(I, M, LI),
    nth0(J, LI, P), !
    );
    criarPonto(-1,-1, P), !.

%! seta o elemento no index I recursivamente

%! Caso base
setaElemento([_|T], 0, elem, [elem|T]) :- !.

%! Caso normal
setaElemento([H|T], K, elem, [H|L]) :-
    KS is K-1,
    setaElemento(T, KS, elem, L).

%! seta P na coordenada i,j
%! Primeiro seta o elemento na lista e
%! após isso seta a nova lista dentro na matriz
setaP(coord(I, J), P, M, ML) :-
    nth0(I, M, L),
    replace(L, J, P, NL),
    replace(M, I, NL, ML), !.

%! Retorna a região do ponto na coordenada
getRAux(C, M, R) :-
    getP(C, M, P),
    getR(P, G),
    findall(
        C1,
        (
            nth0(0, M, L),
            length(M, MI1), length(L, MJ1),
            MI is MI1 - 1, MJ is MJ1 - 1,
            between(0, MI, I), between(0, MJ, J),
            criarCoord(I, J, C1), getP(C1, M, P1), getR(P1, G1), G1 == G
            ),
        R
    ).

getVizinhos(coord(I,J), V) :-
    criarCoord(I,J,CN),
    findall(
        C,
        (   IH is I + 1, IL is I - 1,
            JH is J + 1, JL is I - 1,
            between(IL, IH, I1), between(JL, JH, J1),
            criarCoord(I1, J1, C), C \== CN
        ),
        V
    ).

getRFromLista(L, R) :-
	maplist(getR, L, R).

getVizFromLista(L, LViz) :-
    maplist(getVizFromListaAux, L, LViz).

getVizFromListaAux(coord(I,J), V) :-
    criarCoord(I,J,CN),
    findall(
        C,
        (   IH is I + 1, IL is I - 1,
            JH is J + 1, JL is I - 1,
            between(IL, IH, I1), between(JL, JH, J1),
            criarCoord(I1, J1, C), C \== CN
        ),
        V
    ).

suguru(matriz):-
    matriz(M),
    length(M,6),
    maplist(same_length(M), M).
    
    
sudoku(Rows) :-
    length(Rows, 6),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..6,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
	maplist(all_distinct, Columns).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).
