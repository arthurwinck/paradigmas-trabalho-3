:- use_module(library(clpfd)).

board(
[
    [ponto(4,0),ponto(0,0),ponto(0,2,0),ponto(0,3,0),ponto(0,4,0),ponto(0,5,0)],
    [ponto(1,0,0),ponto(1,1,0),ponto(1,2,0),ponto(1,3,0),ponto(1,4,0),ponto(1,5,0)],
    [ponto(2,0,0),ponto(2,1,0),ponto(2,2,0),ponto(2,3,0),ponto(2,4,0),ponto(2,5,0)],
    [ponto(3,0,0),ponto(3,1,0),ponto(2,3,0),ponto(3,3,0),ponto(3,4,0),ponto(3,5,0)],
    [ponto(4,0,0),ponto(4,1,0),ponto(2,4,0),ponto(4,3,0),ponto(4,4,0),ponto(4,5,0)],
    [ponto(5,0,0),ponto(5,1,0),ponto(2,5,0),ponto(5,3,0),ponto(5,4,0),ponto(5,5,0)]
]
).
¨
%! getters para a região e um valor de um ponto
getR(ponto(v,R), R).
getV(ponto(V,r), V). 

criarPonto(i,v,g, ponto(i,v,g)) :- !.


getPonto(k, board, ponto) :-
    (
        ktoij
        length(board, l1),

    )



getCoord(ponto(C,V),)

suguru(Rows) :-
    length(Rows, 6),
    maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..6,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [A,B,C,D,E,F],
    maplist(checaVizinhos, A),

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

checaVizinhos([],_).
