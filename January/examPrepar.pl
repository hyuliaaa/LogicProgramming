:- use_module(library(clpfd)).

last([X], X).
last([_|T], X) :- last(T, X).

% проверка за хамилтонов граф
is_hamiltonian([V, E]) :-
    permutate(V, [Start | Rest]),
    check_path([V, E], [Start | Rest]),
    last([Start | Rest], End),
    edge([V , E], [End, Start]).

edge([V, E], [X, Y]):- X #<Y, member([X, Y], E).
edge([V,E], [X, Y]):- X #> Y, member([Y, X], E).

check_path([_,_], [_]). % prazen pyt 
check_path([V, E], [X, Y | Rest]):-
    check_path([V, E], [Y, Rest]),
    edge([V, E], [X,Y]).

permutate([H | T], R):- permutate(T, Q), insert(H, Q, R).

insert(H, Q, R) :- append(P, S), append(P, [H | Q], R).

% Генерирайте всички графи над мн-вото N, т.е. върховете ще са подмн-во на N.
% gen_nat_graph(G) -> G = [V, E] is graph and V is finite subset of naturals
% for verticies we have [1,2... N] for some positive N.

nat(0).
nat(N) :- nat(K), N #= K + 1.

gen_nat_graph([V, E]) :-
    nat(N),
    range(1, N, V),
    gen_all_edges(V, All),
    subset(E, All).

% Как да генерираме списъка на всевъзможните ребра
% [1, 2, 3, 4] ->
% 1: [1, 2], [1, 3], [1, 4]
% 2: [2, 3], [2, 4]
% 3: [3, 4]
% 4: [] 
% appendAll -> [ [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4] ]
gen_all_edges([], []).
gen_all_edges([V | VS], All) :-
    gen_all_edges_for_vertex([V | VS], LV),
    gen_all_edges(VS, R), 
    append(LV, R, All).

gen_all_edges_for_vertex([_], []).
gen_all_edges_for_vertex([H | T], L) :-
    T \= [],
    insert_first_to_all(H, T, L).

insert_first_to_all(_, [], []).
insert_first_to_all(X, [H | T], [[X, H] | R]) :-
    insert_first_to_all(X, T, R).

range(A, B, []) :- A #> B.
range(A, B, [A | R]) :- A #=<B, A1 = A + 1, range(A1, B, R).

subset([], []).
subset(S, [_ | T]):- subset(S, T).
ssubset([H|S], [H | T]) :- subset(S, T). % izbirame da q dobavim

gen_nat_ham_graph(G) :-
    gen_nat_graph(G),
    is_hamiltonian(G).
