:- use_module(library(clpfd)).

% join(LS, L) - list of lists to produce a flattened list.
my_append([], L, L).
my_append([H | T], L, [H|R]) :- my_append(T, L, R) .

join([], []).
join([L | LS], R) :- join(LS, T), my_append(L,T,R).

% partition(P, L) : P is onorder partition of L.
partition([], []).
partition([[H]| P], [H | T]) :- partition(P, T).
%partition([[H | HP] | TP], H|T) :-partition([HP|TP], T).


%Arithmetics

% X = 5-1.
% in prolog this gives the answer X = 5-1

% In order to calculate it we must use X is 5-1.

% 5+1 =:= 3+3 -> true '== in Java'

% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=< B.
min(B, A, B) :-A #> B.

% min(M, L) -> M is the minimum element of L(list)
min_el(M, [M]).
min_el(M, [A | T]) :- min_el(K, T), min(M, A, K).

% list_length(L, N) -> N is length of L
list_length([], 0).
list_length([_ | T], N) :- list_length(T, K), N#= K + 1.

% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X | _], 0).
element_at(X, [_ | T], N) :- N#=0, K#=N-1, element_at(X, T, K).

index_in_list(X, [X | _], 0).
index_in_list(X, [H | T], N) :-X#\=H,index_in_list(X, T, K), N#=K + 1.

%is_sorted(L) -: checks whether L is sorted

is_sorted([]).
is_sorted([_]).
is_sorted([H, Y | T]) :- H #=<Y, is_sorted([Y|T ]).

% is_sorted_in_any_order(L)

is_sorted_in_any_order([]).
is_sorted_in_any_order([_]).
is_sorted_in_any_order([H, Y | T]) :-order(H, Y), is_sorted_in_any_order([Y | T]).

% change the order definition and get a completely new function.
order(A,B) :- A#=<B.

% bogo_sort (L) :- generates permutations untill it finds one that is sorted
 
bogo_sort(L, S):-permutate(L, S), is_sorted(S).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P).

insert(X, L, R):- my_append(P, S, L), my_append(P, [X | S], R).


% quick_sort :- we devide a list in two of chosen element, we recursively sort each half, and add in the middle the element that we have chosen.

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


%char_vects(K, VS) :- VS is a list of all characteristics vectors(Vectors with 0/1) with lenght K 
char_vects(0, [[]]).
char_vects(K, VS) :- K#>0, N#=K-1,
                    char_vects(N, TS),
                    insert_first(0, TS, Zeros),
                    insert_first(1,TS, Ones),
                    append(Zeros, Ones, VS).

% poluchavash element i spisyk ot spisaci i kym vseki spicyk dobavash tozi element
insert_first(_, [], []).
insert_first(X, [L | LS], [ [X | L ] |RS ]) :- insert_first(X, LS, RS).


%between(X, A, B) :- checks/generates X(integer) is between A and B.
between(A, A, B) :- A #=<B.
between(X, A, B) :- A #< B, A1 #= A + 1, between(X, A1, B).

%range(L, A, B) :- L= [A, A+1...B]

range([], A, B) :- A#>B.
range([A|L], A, B) :- A #=< B, A1#=A+1, range(L, A1, B).

between_with_range(X, A, B) :-range(R, A, B), member(X, R). 

member(X, [X | _]).
member(X, [_ | T]) := member(X, T).

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)

list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B( [H | T], K, A, B) :- K#>0,
                                                N#=K-1,
                                                between(H, A, B),
                                                list_of_K_elements_between_A_B(T, N, A, B).

% da postroim spisyk s dyljina k s elementi ot nqkakyv spisyk
%variations_with_repetiotions(V, K, L) -: V is list with length K and each element is element of L.
variations_with_repetitions([], 0, _).
variations_with_repetitions([H|T] , K, L) :-
    K#>0,
    N#= K-1,
    member(H, L),
    variations_with_repetitions(T, N, L).

% gen_K_with_sum_S(K,S,L):-L is a list with length K and sum of elements in the list S. 

gen_K_with_sum_S(1, S, [S]):-S#>=0.
gen_K_with_sum_S(K, S,[H|T]):- K#>0, N#=K-1,between(H, 0, S), M#=S-H, gen_K_with_sum_S(N, M, T).  

