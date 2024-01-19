

:- use_module(library(clpfd)).
last1([X], X).
last1([_ | T], X) :- last1(T, X).
% last(X, L) :- append(_, [X], L).

% граф от върхове[V,E] и ребра и имам връх[X,Y] и искаме да проверим, че имаме ребро в графа
edge([_, E],  [X, Y]) :- X #< Y, member([X,Y], E).
edge([_, E],  [X, Y]) :- X #> Y, member([Y, X], E).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P). % добавяме елемента H в Q на произволна позиция и получаваме P
 
insert(X, L, R) :-  append(P, S, L), append(P, [X | S], R). 

check_path([_,_], [_]).
check_path([V, E], [X, Y | Rest ]) :-
    check_path([V, E], [Y | Rest]),
    edge([V, E], [X, Y]). 


% идеята е да генерираме произволна пермутация на върховете  и да вземем първия връх,
% след това да проверим дали имаме път в графа, да вземем последния елемент и да проверим
% дали има ребро от последния към първия.     
is_hamiltonian([V, E]) :-
    permutate(V, [Start | Rest]),
    check_path([V,E], [Start | Rest]),
    last([Start |Rest], End),
    edge([V,E], [End, Start]).

% Генерирайте всички графи над мн-вото N, т.е. върховете ще са подмн-во на N.
% gen_nat_graph(G) -> G = [V, E] is graph and V is finite subset of naturals
% for verticies we have [1,2... N] for some positive N.

nat(0).
nat(N) :- nat(K), N #= K + 1.

gen_nat_graph([V, E]) :-
    nat(N), N #> 0,
    range(1, N, V),
    gen_all_edges(V, All),
    subset(E, All).

range(A, B, []) :- A > B.
range(A, B, [A | R]) :- A1 = A + 1, range(A1, B, R).

subset([], []).
subset(S, [H | T]):- subset(S, T).
subset([H | T], [H | T]) :- subset(S, T).


gen_all_edges_for_vertex([_], []).
gen_all_edges_for_vertex([H | T], L ) :-
    T #\= [],
    insert_first_to_all(H, T, L).


insert_first_to_all(_, [], []).
insert_first_to_all(X, [H | T], [[X | H]], R) :-
    insert_first_to_all(X, T, R).


gen_all_edges([], []).
gen_all_edges([V | VS], All) :-
    gen_all_edges_for_vertex([V | VS], LV),
    gen_all_edges(VS, R),
    append(LV, R, All).
    

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