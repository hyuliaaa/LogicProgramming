edge(E, U, W):- member([U, W], E).

% Всички пътища от U го W с дължина K (не задължително прости a.k.a. ациклични).
pathLengthK(_, W, W, 0).
pathLengthK(E, U, W, K):- 
    K > 0, K1 is K - 1,
    edge(E, U, V),
    pathLengthK(E, V, W, K1).

% Имаме, че G е слабо свързан и K > 1.
pr_Gr(E, K):- 
    not(( edge(E, U, W), 
        not(pathLengthK(E, U, W, K)) )).


commonElement(X, Y, A):- member(A, X), member(A, Y).

condition1(X, Y):- not(commonElement(X, Y, _)).

isSubsetOf([], []).
isSubsetOf(S, [_ | T]) :- isSubsetOf(S , T).
isSubsetOf([H | S], [H | T]) :- isSubsetOf(S, T).

condition2(X, Y):- isSubsetOf(X, Y); isSubsetOf(Y, X).

condition(X, Y):- condition1(X, Y); condition2(X, Y). % [] удовлетворява и двете.

isLaminar(L) :- forall((member(X, L), member(Y, L)), condition(X, Y)).

% isLaminar(L):- not(( member(X, L), member(Y, L), not(condition(X, Y)))).


:-use_module(library(clpfd)).

size([], L) :- L #= 0.
size([_|T], L) :- L#>=0, size(T, L - 1).

nat(N):- N #= 0; nat(N - 1).

s(M) :- nat(N), K #=< N, size(M, 3*K), M ins 0..N, label(M), isSpecific(M).

findAtIndex(I, [X|_], X):- I #= 1.
findAtIndex(I, [_|T], X):- I #> 1, findAtIndex(I - 1, T, X).

isSpecific(X) :- size(X, 3*N), K in 1..N, label([K]), 
findAtIndex(K, X, AK), AK + 6 #=< 3*N, 1 #=< AK + 6, AK #=< K, 1 #=< AK, 
findAtIndex(3*K, X, A3K), 
findAtIndex(AK+6, X, AAk6), A3K #= AAk6.

isSpecific(X) :- size(X, 3*N), K in 1..N, label([K]), 
findAtIndex(K, X, AK), not((AK + 6 #=< 3*N, 1 #=< AK + 6, AK #=< K, 1 #=< AK)), 
findAtIndex(3*K, X, A3K), findAtIndex(3*N - K, X, A3NK), A3K #= 3*A3NK + 1.