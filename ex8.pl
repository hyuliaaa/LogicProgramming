:- use_module(library(clpfd)).

% admissible_graph_color([V, E], K, C) 
% K is the number of colors
% C is a K coloring if list of pairs([U, M])
% U is vertex, M is natural number less than K

% Искаме всеки два върха, ако имаме ребро между тях,
% цветовете им да са различни
% C is K admissible coloring of the vertices if
% (forall U in V)(forall W in V) (<U,W> is in E then C[U] != C[W]).

% and C represents a function from set ot Vertices  V to {0,1..., K-1}
% (forall U in V) (forall M, S in {0,1..., K-1})
% (if [U,M], [U,S] are member of C then M = S)

% проверяваме дали можем да генерираме оцветяване с К на брой цвята
graph_color([], _ ,[]).
graph_color([V | VS], K, [ [V, M] | T]):-
    graph_color(VS, K, T),
    S #= K - 1,
    between(0, S, M).

% (forall U in V)(forall W in V) (<U,W> is in E then C[U] != C[W]).
% not(exists U in V)(exists W in V) not (<U,W> is in E then C[U] != C[W])
% Разписваме импликацията
% not(exists U in V)(exists W in V) not (not(<U,W> is in E) or C[U] != C[W])
% Пак пускаме отрицанието навътре
% not(exists U in V)(exists W in V) (<U,W> is in E) and C[U] == C[W])


gen_K_admissible_coloring([V, E], K, C) :-
    graph_color(V, K, C),
    not((
        member([U, W], E),
        member([U, M], C),
        member([W, S], C),
        M #= S
    )).

edge([U, W], E) :- member([U, W], E).
edge([U, W], E) :- member([W, U], E).

% Искаме да генерираме всевъзможните пътища


% path([V, E], S, F, P) -> P is path in the graph [V, E] between S and F

path([V, E], S, F, P) :-
    subset(M, V),
    member(S, M),
    member(F, M),
    permutate(P, M),
    is_path_valid([V,E], P).

subset([], []).
subset(S, [_| T]) :-subset(S, T).
subset([H | S], [H | T]) :- subset(S, T). 

permutate([], []).
permutate(P, [H | T]) :- permutate(Q, T), insert(H, Q, P).

insert(X, L, R) :- append(P, S, L), append(P, [X|S], R).

is_path_valid([_,_], [_]). % каквито и да са върховете и каквито и да са ребрата, ако имаме път от 1 връх, това е тривиален път
is_path_valid([V, E], [U, W | P]) :-
    edge([U,W], E),
    is_path_valid([V,E], [W | P]).

% Да маркираме посетените върхове

% path_imp([V,E], S, F, P) :- path_imp_track_visited([V,E], S, F, [S], P).


% %  S = 2, F = 3 -> [2, 1, 3]

% % Step 1: [2]
% % edge([2, 1], E),
% % Visited = [1 | [2]] = [1, 2]

% % Step 2: [1, 2]
% % edge([1, 3], E),
% % Visited = [3 | [1, 2]] = [3, 1, 2] -> [2, 1, 3]
% path_imp_track_visited([_,_], F, F, Visited, P):-
%     reverse(Visited, P)
    

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

is_connected([V, E]) :-
    not((
        member(S, V),
        member(F, V),
        S \= F,
        not(path_imp([V, E], S, F, _))
    )).

is_acyclic([V, E]) :-
    not((
        member(S, V),
        member(F, V),
        S \= F,
        path_imp([V, E], S, F, P1),
        path_imp([V, E], S, F, P2),
        P1 \= P2
    )).

is_tree(G) :-
    is_connected(G),
    is_acyclic(G).

