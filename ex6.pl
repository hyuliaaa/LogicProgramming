:- use_module(library(clpfd)).


nat(0).
nat(X) :- nat(N), X #= N + 1.

% Пролог не разбира от за всяко, а само от съществуване.

% is_graph([V,E]) : checks if [V, E] represents a graph
% if (1,2) is edge in the graph, then [1,2] member E
% and [2,1] is not member
% V is sorted

% (forall X member of V)
% (forall Y member of V)
%if [X, Y] is member of E, then X < Y and [Y,X] is not a member of E

% Слагаме 2 отрицания
% not not(forall X member of V) (forall Y member of V)
%if [X, Y] is member of E, then X < Y and [Y,X] is not a member of E

% Пускаме първото отрицание да потъва
% Развиваме импликацията
% not (exists X member of V) (exists Y member of V)
% not (нот [X, Y] is member of E or (X < Y and [Y,X] is not a member of E))

% По ДеМорган
% not (exists X member of V) (exists Y member of V)
% ([X, Y] is member of E and not(X < Y and [Y,X] is not a member of E))

member1(X, [X]).
member1(X, [X | _]).
member1(X, [H | T]) :- X#\=H, member(X, T).

% append([], L, L).
% append([H | T], L, [H | R]) :-
%     append(T, L, R).
    

% is_sorted([]).
% is_sorted([_]).
% is_sorted([X, Y | T]) :- X #=< Y, is_sorted([Y | T]). 

% is_sorted_with_append([]).
% is_sorted_with_append([_]).
% is_sorted_with_append(L) :- append(_, [A, B | _], L), A #>= B.

is_graph1([V, E]) :-
    not((append(_, [A, B | _], V), A #>=B)),
    not((member(X,V), member(Y, V),
    member([X,Y], E), not((X #< Y, not(member( [Y,X], E)))))).

% is_graph([[1,2,3], []]). 
% true.
    
% is_graph([[1,2,1], []]). 
% false.


%Hamiltonian graph - a connected graph that containt Hamiltonian cycle(a path that visits each vertex exactly once
% and goes back to the starting vertex)

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

% permutate & checkpath намират Хамилтонов път
% is_hamiltonian([ [1,2,3,4], [ [1,2], [2,3], [1,3] ] ] ). :- true


% Генерирайте всички графи над мн-вото N, т.е. върховете ще са подмн-во на N.
% gen_nat_graph(G) -> G = [V, E] is graph and V is finite subset of naturals
% for verticies we have [1,2... N] for some positive N.

gen_nat_graph([V, E]) :-
    nat(N), N #> 0,
    range(1, N, V),
    gen_all_edges(V, All),
    subset(E, All).


% range(A, B, L) :- L is a [A, A+1...B].
range(A, B, []) :- A #> B.
range(A, B, [A | R]) :- A #=< B, A1 #= A + 1, range(A1, B, R). 

subset([], []).
subset(S , [_ | T]) :- subset(S, T).
subset([H|S], [H | T]) :- subset(S, T).

% Как да генерираме списъка на всевъзможните ребра
% [1, 2, 3, 4] ->
% 1: [1, 2], [1, 3], [1, 4]
% 2: [2, 3], [2, 4]
% 3: [3, 4]
% 4: [] 
% appendAll -> [ [1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4] ]

% gen_all_edges_for_vertex цели да направи 1: [1, 2], [1, 3], [1, 4]
gen_all_edges_for_vertex([_], []).
gen_all_edges_for_vertex([H | T], L ) :-
    T #\= [], 
    insert_first_to_all(H, T, L).

insert_first_to_all(_, [], []).
insert_first_to_all(X, [H | T],  [ [X, H] | R]):-
    insert_first_to_all(X, T, R).


% V = 1, VS = [2, 3, 4]
% [[1, 2], [1, 3], [1, 4]] . [ [2, 3], [2, 4], [3, 4] ]
gen_all_edges([], []).
gen_all_edges([V | VS], All):-
    gen_all_edges_for_vertex([V|VS], LV),
    gen_all_edges(VS, R),
    append(LV, R, All).


% искаме да генерираме всички графи с ест. числа, които са хамилтонови с точност до изоморфизъм
get_nat_ham_graph(G) :-
    gen_nat_graph(G),
    is_hamiltonian(G).


