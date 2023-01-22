
% path([V, E], S, F, P) -> P is path in the graph [V, E] between S and F

path([V, E], S, F, P) :-
    subseq(M, V),
    member(S, M),
    member(F, M),
    permutate(P, M),
    is_path([V, E], P).

edge([U, W], E) :- member([U, W], E).
edge([U, W], E) :- member([W, U], E).

subseq([], []).
subseq(S , [_ | T]) :- subseq(S, T).
subseq([H | S] , [H | T]) :- subseq(S, T).

permutate([], []).
permutate(P , [H | T]) :- permutate(Q, T), insert(H, Q, P).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

is_path([_, _], [_]).
is_path([V, E], [U, W | P]) :-
    is_path([V, E], [W | P]),
    edge([U, W], E).

%  S = 2, F = 3 -> [2, 1, 3]

% Step 1: [2]
% edge([2, 1], E),
% Visited = [1 | [2]] = [1, 2]

% Step 2: [1, 2]
% edge([1, 3], E),
% Visited = [3 | [1, 2]] = [3, 1, 2] -> [2, 1, 3]

path_imp([_, E], S, F, P) :- path_imp_track_viseted(E, F, [S], P).

path_imp_track_viseted(_, F, [F | Visited], P) :- reverse([F | Visited], P).
path_imp_track_viseted(E, F, [T | Rest], P) :-
    T \= F,
    edge([T, U], E),
    not(member(U, Rest)),
    path_imp_track_viseted(E, F, [U, T | Rest], P).

path([V, E], P) :-
    member(S, V),
    member(F, V),
    S \= F,
    path_imp([V, E], S, F, P). 

reverse([], []).
reverse([H | T], R) :- reverse(T, Temp), append(Temp, [H], R).


is_connected([V, E]):-
    not((
        member(S, V),
        member(F, V),
        not(path_imp([V, E], S, F, _))
    )).


is_acyclic([V, E]):-
    not((
        member(S, V),
        member(F, V),
        path_imp([V,E], S, F, P1),
        path([V,E], S, F, P2),
        P1 \= P2
    )).