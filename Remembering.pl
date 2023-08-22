:- use_module(library(clpfd)).

% check whether a smt is a list
list([]).
list([ _ | T]) :- list(T).

% check wheter X is element of L
member(X, [X | _]).
member(X, [Y | T]) :- member(X, T).

% append (L1, L2, R) :- L1 . L2
append([], L2, L2).
append([H | T], L2, [H | R]) :-append(T, L2, R).

first(X, [X | _]).

first_with_append(F, L) :- append([F], _, L).

last(X, [X]).
last(X, [_ | T]) :- last(X, T).

last_with_append(X, L) :- append(_, [X], L).

% prefix(P, L) :- P is prefix of L
prefix(P, L) :- append(P, _, L).

suffix(S, L) :- append(_, S, L).    

sublist(S, L) :- prefix(P, L), suffix(S, P).


reverse([], []).
reverse(Result, [H | T]) :- reverse(Temp, T), append(Temp, [H], Result).

% palyndrome (L) is true IFF L is palyndrome
palyndrome(L) :- reverse(L, L).

% remove(X, L, R) - remove first occurence of X in L and return R
remove(X, [X | T], T).
remove(X, [H | T], [ H | R]):- remove(X, T, R), X\=H.

%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T],R) :- remove_all(X, T, R).
remove_all(X, [Y | T], [Y | R ]) :- remove_all(X, T, R), X\=Y.

%insert(X, L, R) - R is L in which X is inserted in some position
% we insert the element in every possible place.
insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% permutate(P, L) - P to be permutation of L
permutate([], []).
permutate(P, [H | T]) :- permutate(Q, T), insert(H, Q, P).


%subsequence(S, L) - S is a subsequence of L, the order should be the same
%exampple : subsequence([b,d], [a,b,c,d,e]). - in even places
%subsequence([a,c,e], [a,b,c,d,e]).

subsequence([], []).
subsequence(S, [_ | T]) :- subsequence(S, T). % we dont take the first element
subsequence([H | S], [H | T]) :- subsequence(S, T). % we take the first element

list_length([], 0).
list_length([_ | T], N) :- list_length(T, K), N #= K + 1.
