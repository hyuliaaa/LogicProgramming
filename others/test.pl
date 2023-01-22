:- use_module(library(clpfd)).

list([]).
list( [_| T]) :- list(T).

my_member(X, [X | _]).
my_member(X, [H | T]) :- X #\=H, my_member(X, T).

my_append([], L, L).
my_append([H | T], L, [H | R]) :- my_append(T, L, R).

my_replace(_, _, [], []).
my_replace(X, Y, [X | T], [Y | R]) :- my_replace(X, Y, T ,R). 
my_replace(X, Y, [H | T], [H |R ]) :- my_replace(X, Y, T, R), X #\= H.

first(X, [X| _]).

last1(X, [X]).
last1(X, [_| T]):- last1(X,T).

last_with_append(X, L) :-append(_, [X], L).

prefix(P, L):- append(P, _, L).
suffix(S, L):- append(_, S, L).


sublist(S, L):- prefix(P, L), suffix(S, P).

reverse([], []).
reverse(Result, [H | T]):- reverse(R, T), my_append(R, [H], Result).

palyndrome(P, L) :- reverse(R, L), append(R, [], P).

is_pal(L) :-reverse(L, L).

% remove first occurence of X in L
% remove(X, L, R)

remove(_, [], []).
remove(X, [X|T], T).
remove(X, [H | T], [H | R]):- X#\=H, remove(X, T, R).

remove_all(_, [], []).
% remove_all(X, [X], []).
remove_all(X, [X|T], R):- remove_all(X, T, R).
remove_all(X, [H | T], [H | R]):- X#\=H, remove_all(X, T, R).


insert(X, L, R):- append(P, S, L), append(P, [X|S], R).

permutate([], []).
permutate(P, [H | T]) :- permutate(Temp, T), insert(H, Temp, P).

subsequence([], []).
subsequence(S, [_ | T]) :-subsequence(S, T).
subsequence([H | S], [H | T]) :- subsequence(S, T).

% remove_duplicates([], []).
% remove_duplicates([H|T], R) :-
%     member(H, T),
%     remove_duplicates(T, R).

remove_duplicates([],[]).

remove_duplicates([H | T], [H|T1]) :- 
    not((member(H, T))),
    remove_duplicates( T, T1).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates(T, List).

power_set([P], S):- sublist(P, S).

join([], []).
join([L | LS], R) :- join(LS, T), append(L, T, R).


%partition(P, L).
partition([], []).
partition([[H]| P], [H | T]) :- partition(P, T).


min(A, A, B) :- A #=< B.
min(B, A, B) :-A #> B.

%min_el(M, L).
min_el(X, [X]).
min_el(X, [H |T]) :- min_el(Min, T),min(X, Min, H). 

list_length([], 0).
list_length([_ | T], N) :- list_length(T, K), N #= K + 1.  


%element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X|_], 0).
element_at(X, [_ | T], P) :- element_at(X, T, Temp), P #= Temp + 1.

is_sorted([]).
is_sorted([_]).
is_sorted([H , Y | T]) :- H #=<Y, is_sorted([Y | T]).

bogo_sort(L, S) :- permutate(S, L), is_sorted(S).


% set predicates

in_union(X, A, B) :- member(X, A); member(X, B).
in_intersection(X, A, B) :- member(X, A), member(X, B).
in_difference(X, A, B) :-member(X, A), not((member(X, B))).


is_subset_of(A, B) :- member(X, B), not((member(X, A))).

% is_subset_of(A, B):- \+((member(X, A), \+(member(X, B)))).



quick_sort([], []).
quick_sort([H | T], Sorted ):-
    split(T, H, Less, Bigger),
    quick_sort(Less, Sorted_Less),
    quick_sort(Bigger, Sorted_Bigger),
    my_append(Sorted_Less,  [H|Sorted_Bigger], Sorted). 

%split(List, pivot, bigger, less).
%add(H, L, Less, Bigger) -: it will determine to which to add H Less or Biggger
split([], _, [], []).
split([H | T], X, Less, Bigger ) :-split(T, X, L, B), add(H, X, L, B, Less, Bigger).

add(E, X, L, B, [E|L], B):- order(E, X). % if E < X, add to Less
add(E, X, L, B, L, [E|B]) :-not(order(E,X)).

order(A,B) :- A#=<B.



% poluchavash element i spisyk ot spisaci i kym vseki spicyk dobavash tozi element
insert_first(_, [], []).
insert_first(X, [L | LS], [[X | L]| RS]) :- insert_first(X, LS, RS).


%between(X, A, B) :- checks/generates X(integer) is between A and B.

between(A, A, B) :- A #=<B.
between(X, A, B) :- A #< B, A1 #= A + 1, between(X, A1, B).


range([], A, B) :- A #> B.
range([A |R], A, B) :- A#=<B, A1 #= A + 1, range(R, A1, B).

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)

list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B([X|R], K, A, B) :-
    K #>0,
    N #= K - 1,
    between(X, A, B),
    list_of_K_elements_between_A_B(R, N, A, B). 


% da postroim spisyk s dyljina k s elementi ot nqkakyv spisyk
%variations_with_repetiotions(V, K, L) -: V is list with length K and each element is element of L.
variations_with_repetiotions([], 0, _).
variations_with_repetiotions([H | T], K, L) :-
    K #> 0,
    N#= K - 1,
    member(H, L),
    variations_with_repetiotions(T, N, L).

% gen_KS(K,S,L):-L is a list with length K and sum of elements in the list S. 

gen_KS(1, S, [S]):- S#>=0.
gen_KS(K, S, [H |R ]) :-
    K #> 1,
    between(H, 0, S),
    O #= S - H,
    N #= K - 1,
    gen_KS(N, O, R).


% gen_K_with_sum_S(1, S, [S]):-S#>=0.
% gen_K_with_sum_S(K, S,[H|T]):- K#>0, N#=K-1,between(H, 0, S), M#=S-H, gen_K_with_sum_S(N, M, T).  


nat(0).
nat(N) :- nat(K), N#=K+1.


gen_pair_of_nats(X, Y) :- 
    nat(Sum),
    my_between(0, Sum, X),
    Y #= Sum - X.

gen_pair(X, Y) :-
    nat(Sum),
    gen_KS(2, Sum, [X,Y]).

my_between(A, B, A):-A#=<B.
my_between(A, B,  R) :- A#<B, A1#=A+1, my_between(A1, B, R).


% генерирайте всички елементи на N^k
gen_Nat_K(K, T) :-
    nat(Sum),
    gen_KS(K, Sum, T).



int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.



% генерира всички крайни редици от ест. числа, които са с ненулева дължина
% gen_fin_seq_Nat(L) :- % Канторово изброяване
gen_fin_seq_Nat(L) :-
    gen_pair(K, S),
    gen_KS(K, S, L).


% генерирайте всевъзможните крайни подмн-ва на ест. числа
%gen_fin_subset_Nat(S).

gen_fin_subset_Nat([]). % празното мн-во за нас е празният списък
gen_fin_subset_Nat(S) :-
    gen_fin_seq_Nat(S),
    encodes_subset(S).

%  ще позволяваме да генерираме списъци, където елементите са подредени и то в строго растящ ред
encodes_subset([]).
encodes_subset([_]).
encodes_subset([A, B | T ]) :-
    A #< B,
    encodes_subset([B | T]).


% да се генерират всевъзможните аритметични прогресии от ест. числа
%gen_arr_prog(P).

gen_arr_prog([]).
gen_arr_prog([_]).
gen_arr_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, A0, D]).


%Hamiltonian Graph
last_in_list(L, X) :- append(_, [X], L).

% граф от върхове[V,E] и ребра и имам връх[X,Y] и искаме да проверим, че имаме ребро в графа

edge([_, E], [X, Y]) :- X #< Y, member([X, Y], E).
edge([_, E], [X, Y]) :- Y #< X, member([Y, X], E).

permutateList([], []).
permutateList([H | T], P):- permutateList(T, Q), insert(H, Q, P).

check_path([_, _], [_]).
check_path([V, E], [X, Y | Rest]) :-
    check_path([V, E], [Y | Rest]),
    edge([V, E], [X, Y]).


% идеята е да генерираме произволна пермутация на върховете  и да вземем първия връх,
% след това да проверим дали имаме път в графа, да вземем последния елемент и да проверим
% дали има ребро от последния към първия.     
is_hamiltonian([V, E]) :-
    permutate(V, [Start | Rest]),
    check_path([V, E], [Start | Rest]),
    last_in_list([Start | Rest], End),
    edge([V, E], [End, Start]).