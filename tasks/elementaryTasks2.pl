:- use_module(library(clpfd)).

% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=< B.
min(B, A, B) :- A #> B.

% min(M, L) -> M is the minimum element of L(list)
minimum(M, [M]).
minimum( M, [H | T]) :- minimum(MCurr, T), min(M, H, MCurr).

% list_length(L, N) -> N is length of L
list_length([], 0).
list_length([H | T], N) :- list_length(T, K), N #= K + 1.

% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X | _], 0).
element_at(X, [_ | T], N) :- N #> 0, K #= N - 1, element_at(X, T, K).

%index_in_list
index_in_list(X, [X | _], 0).
index_in_list(X, [H | T], N) :-X#\=H,index_in_list(X, T, K), N#=K + 1.

%is_sorted(L) -: checks whether L is sorted
is_sorted([]).
is_sorted([_]).
is_sorted([H, Y | T]) :- is_sorted([Y | T]), H #=< Y. 

% is_sorted_in_any_order(L)
is_sorted_in_any_order([]).
is_sorted_in_any_order([_]).
is_sorted_in_any_order([H, Y | T]) :- order(H, Y), is_sorted_in_any_order([Y | T]).

order(H, Y) :- H #< Y.

% bogo_sort (L, S) :- generates permutations untill it finds one that is sorted
append([], L, L).
append([H | L1 ], L2, [H | Curr] ) :- append(L1, L2, Curr).

insert(X, L, R):- append(P, S, L), append(P, [X | S], R).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P).

bogo_sort(L, S):-permutate(L, S), is_sorted(S).

%between(X, A, B) :- checks/generates X(integer) is between A and B.
between(A, A, B) :- A #=<B.
between(X, A, B) :- A #< B, A1#= A + 1, between(X, A1, B).

%range(L, A, B) :- L= [A, A+1...B]
range([], A, B) :- A #> B.
range([A | R], A, B) :- A #=<B, A1 #= A + 1, range(R ,A1, B).

member(X, [X | _]).
member(X, [_ | T]) :- member(X, T).

between_with_range(X, A, B) :-range(R, A, B), member(X, R). 

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)
list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B([H|T], K, A, B) :- K #>= 0,
                                                  between(H, A, B),
                                                  N #= K - 1,
                                                  list_of_K_elements_between_A_B(T, N, A, B).


% da postroim spisyk s dyljina k s elementi ot nqkakyv spisyk
%variations_with_repetiotions(V, K, L) -: V is list with length K and each element is element of L.
                                         
 variations_with_repetitions([], 0, _).
 variations_with_repetitions([H | T], K, L) :-  K #> 0,
                                                N #= K - 1,
                                                member(H, L),
                                                variations_with_repetitions(T, N, L).
                                                
list_of_K_elements_between_A_and_B_with_range(L, K, A, B) :- range(R, A, B), variations_with_repetitions(L, K, R).

% gen_KS(K, S, L) -> L is list with length K and elements naturals with sum S.
gen_KS(1, S, [S]) :- S#>=0.
gen_KS(K, S, [H | T]) :- K>=1, 
                    N #= K - 1,
                    M #= S - H,
                    between(H, O, S),
                    gen_KS(N,M,T).