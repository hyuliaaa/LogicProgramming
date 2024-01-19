% check whethe smth is a list
list([]).
list([_| T]) :- list(T).

member3(X, [X | _]).
member3(X, [H | T]) :- member3(X, T).

append3([], L2, L2).
append3([H | T], L2, [H | R]) :- append3(T, L2, R).

% first(X, L) - X is the first element of L
first(X, [X | _]).

% last(X, L) - X is the last element of L
last(X, [X]).
last(X, [H | T]) :- last(X, T).

%last with append
last_with_append(X, L) :- append3(_, [X], L).

% prefix(P, L) :- P is prefix of L
prefix(P, L) :- append3(P, _, L).

% suffix (S, L) - where S is suffix of L
suffix(S, L) :- append3(_, S, L).

% sublist(S, L) - S is a sublist of L
sublist(S, L):-prefix(P, L), suffix(S, P).

% reverse(R, L) - R is the reverse list of L
reverse([], []).
reverse(R, [H | T]) :- reverse(K, T), append3(K, [H], R).

reversed([], []).
reversed(Res, [H | T]) :- reversed(R, T), append(R, [H], Res).

palyndrome(L) :- reverse(L, L).

% remove(X, L, R) - remove first occurence of X in L and return R
remove(X, [X | T], T).
remove(X, [H | T], [H | R]) :- remove(X, T, R), X \= H.

%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).

remove_all(X, [X | T], R) :- remove_all(X, T, R).
remove_all(X, [H | T], [H |R ]) :- remove_all(X, T, R), X \= H.

%insert(X, L, R) - R is L in which X is inserted in some position
insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

% permutate(P, L) - P to be permutation of L
permutate([], []).
permutate(P, [H | T]) :- permutate(Q, T), insert(H, Q, P).


%subsequence(S, L) - S is a subsequence of L, the order should be the same
%exampple : subsequence([b,d], [a,b,c,d,e]). - in even places
%subsequence([a,c,e], [a,b,c,d,e]).
subsequence([], []).
subsequence(S, [_ | T]):- subsequence(S, T).
subsequence([H | S], [H | T]) :-subsequence(S, T).


% четнопозиционирани(X, Y) - Y е списък от елементите на четна
%                            позиция в X

evenpos([], []).
evenpos([_], []).
evenpos([H, Y | T], [Y | R]) :- evenpos(T, R).

oddpos([], []).
oddpos([X], [X]).
oddpos([H, Y | T], [H | R]) :- oddpos(T, R).

predposleden(X, [X, Y]).
predposleden(P, [H | T]) :- predposleden(P, T).

predposleden2(A, X) :- append(_, [A,_], X).

tretiposleden(A, X) :- append(_, [A,_,_], X).

% равна_дължина(X, Y) - списъците X и Y имат равна дължина
equal_lengths([], []).
equal_lengths([_|X], [_ | Y]) :- equal_lengths(X, Y).