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