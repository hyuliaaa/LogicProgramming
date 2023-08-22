:- use_module(library(clpfd)).


% check whether 5 smt is a list
list([]).
list([_|T]) :-list(T).


flatten([], []).
flatten(X, [X]):- not(list(X)).
flatten([H|T], R):- flatten(H, FH), flatten(T, FT), 
	append(FH, FT, R).

% check wheter X is element of L
member3(X, [X | _]).
member3(X, [Y | T]) :-member3(X, T).

% append (L1, L2, R) :- L1 . L2
% to the tail of first we add the second list and as a final step we add the head
append1([], L2, L2).
append1([H | T], L2, [H | R]) :- append1(T, L2, R).

% first(X, L) - X is the first element of L
my_first(X, [X |_]).

% last(X, L) - X is the last element of L
my_last(X, [X]).
my_last(X, [_| T]) :- my_last(X, T).

%last with append
last_append(X, L) :- append1(_, [X], L).

% prefix(P, L) :- P is prefix of L
prefix(P, L) :-append(P, _, L).

% suffix (S, L) - where S is suffix of L
suffix(S, L) :- append(_, S, L).

% sublist(S, L) - S is a sublist of L
sublist(S, L) :- prefix(P, L), suffix(S, P).

% reverse(L, R) - R is the reverse list of L
reverse([], []).
reverse([H | T], R) :- reverse(T, Temp), append(Temp, [H], R).

% palyndrome (L) is true IFF L is palyndrome
palyndrome(L) :- reverse(L,L).

remove(X, [X | T], T).
remove(X, [H | T], [H |R]) :- remove(X, T, R), X\=H.

% remove(X, L, R) - remove first occurence of X in L and return R
remove(X, [X], []).
remove(X, [X | T], T).
remove(X, [H | T], [X|R]) :- remove(X, T, R), X \= H.

%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T], R):- remove_all(X, T, R).
remove_all(X, [Y | T], [Y|R]) :- remove_all(X, T, R), X \= Y.

%insert(X, L, R) - R is L in which X is inserted in every possible position

%append (P,S,L) -> partition L in two lists P,S
insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% permutate(P, L) - P to be permutation of L
permutate([], []).
permutate(P , [H | T]) :- permutate(Q, T), insert(H, Q, P).

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