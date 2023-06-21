:- use_module(library(clpfd)).

% member(H, [H | _]). % H is member if it the first element.
% member(X, [_ | T]) :- member(X, T). % X is member if it element of the tail of the list (rest)

% append([], L, L).
% append([H | T], L2,  [H | R]):- append(T, L2, R).

% For graphs we determine ordered graphs
% По ДеМорган
% not ((exists X member of V) (exists Y member of V)
% ([X, Y] is member of E and not(X < Y and [Y,X] is not a member of E)))

is_graph([V, E]) :-
    not((append(_, [A, B | _], V), A #>=B)),
    not((member(X, V), member(Y, V),
    member([X,Y], E), not(( X#<Y, not(member([Y,X], E)))))).


% 28 ?- is_graph([[1,3,2], []]). 
% false.

% 29 ?- is_graph([[1,2,3], []]). 
% % true. 

% 52 ?- is_graph([[1,2,3], [[1,2]]]).
% true.

% 53 ?- is_graph([ [1,2,3], [ [2,1] ]]). 
% false.



%Hamiltonian graph - a connected graph that containt Hamiltonian cycle(a path that visits each vertex exactly once
% and goes back to the starting vertex)

is_hamiltonian([V,E]) :-
    permutate(V, [Start | Rest]),
    check_path([V, E], [Start | Rest]), % давам му графа и му давам съотвено целия път , това ще подсигури, че имам Хамилтонов път
    last_in_list([Start | Rest], End), % от рест взимам последния, но трябва да взимаме и start понеже можем да имаме само 1 връх    
    edge([V,E], [End, Start]).

last_in_list(L, X) :- append(_, [X], L).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P). % добавяме елемента H в Q на произволна позиция и получаваме P

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% граф от върхове[V,E] и ребра и имам връх[X,Y] и искаме да проверим, че имаме ребро в графа
edge([_, E], [X, Y]) :- X #<Y, member([X, Y], E).
edge([_, E], [X, Y]) :- X #> Y, member([Y, X], E).


check_path([_, _], [_]).
check_path([V, E], [X, Y | Rest]) :-
    check_path([V, E], [Y | Rest]),
    edge([V,E], [X, Y]).

% Генерирайте всички графи над мн-вото N, т.е. върховете ще са подмн-во на N.
% gen_nat_graph(G) -> G = [V, E] is graph and V is finite subset of naturals
% for verticies we have [1,2... N] for some positive N.