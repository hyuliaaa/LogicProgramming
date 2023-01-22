:- use_module(library(clpfd)).
list([]).
list([_,_]).

my_member(X, [X | _]).
my_member(X, [_ | T]) :- my_member(X, T).

my_append([], L2, L2).
my_append([H | T], L2, [H | R]):- my_append(T, L2, R).

% replace(X, Y, L, R).
replace(_, _, [], []).
replace(X, Y, [X | T], [Y | T]).
replace(X, Y, [H | T], [H | R]) :- X =\= H, replace(X, Y, T, R).


first([X|_], X).
first_with_append(L, X) :- append([X], _, L).

last_with_append(L, X) :- append(_, [X], L).

prefix(P, L) :- append(P, _, L).

suffix(S, L) :- append(_, S, L).

sublist(S, L) :- prefix(P, L), suffix(S, P).

reverse([], []).
reverse(R, [H | T]) :- reverse(TR, T), append(TR, [H], R).

palyndrome(L) :- reverse(L, L).


% remove(X, L, R) - remove first occurence of X in L and return R
remove(_, [], []).
remove(X, [X | T], T).
remove(X, [H | T], [H | R]):- X =\= H, remove(X, T, R).


remove_all(_, [], []).
remove_all(X, [X | T], R ) :- remove_all(X, T, R).
remove_all(X, [H | T], [H | R]):- X =\= H, remove_all(X, T, R).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

permutate([], []).
permutate(P, [H | T]) :- permutate(Temp, T), insert(H, Temp, P).

subsequence([], []).
subsequence(S, [_ | T]) :- subsequence(S, T).
subsequence([H |S], [H | T]) :- subsequence(S, T).


% power_set(P,S) - P is the power set of S (S has no repetitions)
power_set([[], []]). %stepenneto mn-vo na [], [[]].
power_set(P, [A | S]) :-
    power_set(B, S), 
    prepend_to_all(A, B, C),
    append(B, C, P).

prepend_to_all(_, [], []).
prepend_to_all(X, [L | LS], [[X | L] | RS]):- prepend_to_all(X, LS, RS).

join([], []).
join([L | LS], R):- join(LS, Temp), append(L, Temp, R ).

% partition(P, L) : P is onorder partition of L.

partition([], []).
partition([ [H]| P  ], [H | T]) :- partition(P, T).
partition([ [H | HP] | TP ], [H | T]) :- partition([HP | TP], T).


partition_with_append([], []).
partition_with_append( [P | PS], L):-
    append(P, S, L),
    P \= [],
    partition_with_append(PS, S).


min(A, B, A) :- A =< B.
min(A, B, B):- A > B.


min_el(M, [M]).
min_el(M, [A, B | T]) :- min_el(K, [B | T]), min(A, K, M).


min_el(M, [M]).
min_el(M, [A | T]) :- min_el(K, T), min(M, A, K).

list_length([], 0).
list_length([_| T], N) :- list_length(T, M), N #= M + 1. 

% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X | _],  0).
element_at(X, [_ | T], N):- N #> 0, K#= N - 1, element_at(X, T, K).


index_in_list(X, [X | _], 0).
index_in_list(X, [H | T], N) :-X#\=H,index_in_list(X, T, K), N#=K + 1.


quick_sort([], []).
quick_sort([H | T], Sorted) :-
    split(T, H, Less, Bigger),
    quick_sort(Less, Sorted_Less),
    quick_sort(Bigger, Sorted_Bigger),
    append(Sorted_Less, [H | Sorted_Bigger], Sorted).

split([], _, [], []).
split([H | T], P, Less, Bigger) :- split(T, P, L, B), add(H, P, L, B, Less, Bigger).

%add(H, Pivot, L, B, Less, Bigger) -: it will determine to which to add H Less or Biggger
add(H, X, L, B, [H|L], B):- order(H, X). % if E < X, add to Less
add(H, X, L, B, L, [H|B]) :-not(order(H,X)).

order(A,B) :- A#=<B.



insert_first(_,[], []).
insert_first(X, [L | LS], [[X|L]| RS]):- insert_first(X, LS, RS).

between(A, A, B):- A #=<B.
between(X, A, B) :- A #=<B, A1 #= A + 1, between(X, A1, B).


range([], A, B):- A #> B.
range([A | R],A, B):- A #=<B, A1 #= A + 1, range(R, A1, B).


between_with_range(X, A, B) :- range(R, A, B), member(X, R).


% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)

list_of_K_elements_between_A_B([], 0, _, _).

list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B([H | T], K, A, B):-
    K #> 0,
    N#= K - 1,
    between(H, A, B),
    list_of_K_elements_between_A_B(T, N, A, B).


% da postroim spisyk s dyljina k s elementi ot nqkakyv spisyk
%variations_with_repetitions(V, K, L) -: V is list with length K and each element is element of L.
variations_with_repetitions([], 0, _).
variations_with_repetitions([H|T] , K, L) :-
    K#>0,
    N#= K-1,
    member(H, L),
    variations_with_repetitions(T, N, L).


% gen_K_with_sum_S(K,S,L):-L is a list with length K and sum of elements in the list S. 
gen_KS(1, Sum, [Sum], [S | R]).
gen_KS(K, S, []) :-
    K #> 0,
    K1 #= K - 1,
    between(0, S, H),
    SH #= S - H,
    gen_KS(K1, SH, R).