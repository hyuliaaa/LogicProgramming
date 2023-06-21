:- use_module(library(clpfd)).

is_list([]).
is_list([_ | T]) :- is_list(T).

member3(X, [X | T]).
member3(X, [H | T]) :- member3(X, T).

append1([], L, L).
append1([H | T], L, [H | R]) :- append1(T, L, R).

% replace(X, Y, L, R) :- replace X with Y in L to result R

replace2(_, _, [], []).
replace2(X, Y, [X | T], [Y | R]) :-replace2(X, Y, T, R).
replace2(X, Y, [Z | T], R) :- replace2(X, Y, T, R), X\=Z.

% first(X, L) - X is the first element of L
first2(X, [X | _]).

% last(X, L) - X is the last element of L
last(X, [X]).
last(X, [Y | T]) :-last(X, T).


%last with append
last_app1(X, L) :- append1(_, [X], L).

% prefix(P, L) :- P is prefix of L
prefix1(P, L) :- append1(P, _, L).


suffix1(S, L) :- append1(_, S, L).

% sublist(S, L) - S is a sublist of L

sublist(S, L) :- prefix(P, L), suffix1(S, P).


% reverse(R, L) - R is the reverse list of L
reverse2([], []).
reverse2(S , [H | T]) :- reverse2(R , T), append1(R, [H], S).

% palyndrome (L) is true IFF L is palyndrome
palyndrome(L) :- reverse2(L, L).

% remove(X, L, R) - remove first occurence of X in L and return R
remove1(X, [X | T], T).
remove1(X, [Y | T], [Y | R]) :- remove1(X, T, R), X\=Y.


%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T], R) :- remove_all(X, T, R).
remove_all(X, [Y | T], [Y | R] ) :- remove_all(X, T, R), X\=Y.

%insert(X, L, R) - R is L in which X is inserted in some position
insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).


% permutate(P, L) - P to be permutation of L
permutate([], []).
permutate(P, [H | T]) :- permutate(Q, T), insert(H, Q, P).

%subsequence(S, L) - S is a subsequence of L, the order should be the same
%exampple : subsequence([b,d], [a,b,c,d,e]). - in even places
%subsequence([a,c,e], [a,b,c,d,e]).


subsequence([], []).
subsequence(S, [_ | T]) :- subsequence(S , T).
subsequence([H | S], [H | T]) :- subsequence(S, T).

% power_set(P,S) - P is the power set of S (S has no repetitions)
power_set([[], []]). %stepenneto mn-vo na [], [[]].
power_set(P, [A | S]) :-
    power_set(B, S), % we follow the def so we gen the P(S) to be B
    prepend_to_all(A, B, C), % then we prepend A to B
    append(B, C, P). % and then we make union of B and C to produce P

prepend_to_all(_, [], []).
prepend_to_all(X, [L | LS], [[X | L] | RS]):- prepend_to_all(X, LS, RS).


% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=< B.
min(B, A, B) :- B #< A. 

% min(M, L) -> M is the minimum element of L(list)
min(X, [X]).
min(M, [H | T]) :-min(CM, T), min(M, CM, H).

% list_length(L, N) -> N is length of L
len([], 0).
len([H | T], R) :- len(T, C), R #= C + 1.


% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X | _], 0).
element_at(X, [_ | T], N) :- N #> 0, K #= N - 1, element_at(X, T, K).


index_in_list(X, [X | _], 0).
index_in_list(X, [H | T], N) :-X#\=H, index_in_list(X, T, K), N#=K + 1.



%is_sorted(L) -: checks whether L is sorted
is_sorted([]).
is_sorted([X]).
is_sorted([H, Y | T]) :- H #=<Y, is_sorted([Y|T]).


%between(X, A, B) :- checks/generates X(integer) is between A and B.
between(A, A, B) :- A #=< B.
between(X, A, B) :- A #=<B, A1 #= A + 1, between(X, A1, B).

%range(L, A, B) :- L= [A, A+1...B]
range([], A, B) :- A #> B.
range([A | R], A, B) :- A #=<B, A1 #= A + 1, range(R, A1, B).

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)