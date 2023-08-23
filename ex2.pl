% check whether a smt is a list
list([]).
list([H | T]) :- list(T).

% check wheter X is element of L
member1(X, [X]).
member1(X, [X | _]).
member1(X, [H | T]) :- member1(X, T).

% way 2
my_member(X, [X | _]).
my_member(X, [H | T]) :- my_member(X, T).

% append (L1, L2, R) :- L1 . L2
append1([], L2, L2).
append1([H | T], L2, [H | R]) :- append1(T, L2, R).

% The next row just decides to leave the a -> [ a | []] 

% first(X, L) - X is the first element of L
first1(X, [X | _]).

% last(X, L) - X is the last element of L
last1(X, [X]).
last1(X, [_ | T]) :- last1(X, T).

% append1([], L2, L2).
% append1([H | T], L2, [H | R]) :- append1(T, L2, R).

%last with append
last_app(X, L) :- append1(_, [X], L).

% prefix(P, L) :- P is prefix of L
prefix(P, L) :- append(P, _, L).

% suffix (S, L) - where S is suffix of L
suffix(S, L) :- append(_, S, L).

% sublist(S, L) - S is a sublist of L
sublist(S, L) :- prefix(P, L), suffix(S, P). %we take the suffix of the prefix 

% reverse(R, L) - R is the reverse list of L
reverse1([],[]).
reverse1(Result , [Head | Tail]) :- reverse1(Temp, Tail), append(Temp, [Head] ,Result).

% palyndrome (L) is true IFF L is palyndrome
palyndrome(L) :- reverse1(L, L).

% remove(X, L, R) - remove first occurence of X in L and return R
remove(X, [X | T], T).
remove(X, [H | T], [H |R]) :- remove(X, T, R), X\=H.

%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T], R) :-remove_all(X, T, R).
remove_all(X, [H |T], [H | R]) :- remove_all(X, T, R),  X\=H.

%insert(X, L, R) - R is L in which X is inserted in some position
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
power_set([[]], []). %stepenneto mn-vo na [], [[]].
power_set(P, [A | S]) :-
    power_set(B, S), % we follow the def so we gen the P(S) to be B
    prepend_to_all(A, B, C), % then we prepend A to B
    append(B, C, P). % and then we make union of B and C to produce P

prepend_to_all(_, [], []).
prepend_to_all(X, [L | LS], [[X | L] | RS]):- prepend_to_all(X, LS, RS).