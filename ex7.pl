:- use_module(library(clpfd)).

% Искаме да проверим дали списък от списъци L и друг списък T, дали T е трансверзала на L
% is_list_transversal(L, T), where L is list of lists, T is list
% and T is transversal for L.

% M is a set of sets
% R is transversal for M if 
% (forall S in M) (S intersect R is not empty set)

% (forall X member of L) (exists A: A is member of X and A is member of T)
is_list_transversal([], _). % понеже списъкът е празен, можем да вземем какъвто си искаме елемент на празния списък,защото няма елементи
is_list_transversal([X | XS], Transversal) :-
    member(A, X),
    member(A, Transversal),
    !,
    is_list_transversal(XS, Transversal).

% (forall X member of L) (exists A: A is member of X and A is member of T)
% слагаме not not
% not(exists X member of L ) not(exists A: A is member of X and A is member of T)   

is_list_transversal_with_Not(L, T) :-
    not((member(X, L), not((member(A,X), member(A, T))))).

% gen_list_transversal(L, T) -> we generate T such that it is list transversal of L(L is list of lists).
% (forall E in T)(E is member of some element of L)
% (forall E in T)(E is member of union of the elements of L)
% T is subsequence of (union of the elements of L)
gen_list_transversal(L, T) :-
    union_of_elements(U, L), % U is the union of elements of L
    subsequence(T, U), % T is subsequnce of U
    is_list_transversal(L, T).

% join([], []).
% join(U, [L | LS]) :- join(R, LS), append(L, R, U).

union_of_elements([], []).
union_of_elements(U, [L | LS]) :-
    union_of_elements(R, LS),
    set_minus(L, R, Temp),
    append(Temp, R, U).

% set_minus(S, M, T) -> T = S \ M.
% set_minus(S, [], S).
% set_minus(S, [X | R], U) :-
%    set_minus(S, R, Y),
%    remove(X, Y, U).

set_minus([], _, []).
set_minus([X | S], M, R) :-
    set_minus(S, M, R),
    member(X, M).
set_minus([X | S], M, [X | R]) :-
    set_minus(S, M, R),
    not(member(X, M)).

% set_minus([], _, []).
% set_minus([X | S], M, U) :-
%     set_minus(S, M, R),
%     conditional_add(X, R, M, U).

% % conditional_add(X, A, B, R) -> R is (A union B \ [X])
% conditional_add(X, A, B, A) :- member(X, B).
% conditional_add(X, A, B, [X | A]) :- not(member(X, B)).


divide([], []).
divide(L, [H|R]):- append(H, T, L), H \= [], divide(T, R).


subsequence([], []).
subsequence(S, [_ | T]) :-
    subsequence(S, T).
subsequence([H | S], [H | T]) :-
    subsequence(S, T).


% M is minimal transversal for set of sets S.
% 1. M is transversal
% 2. (forall X in P(M)) (X is transversal => X = M)

% M is minimal transversal for set of sets S.
% 1. M is transversal
% 2. (forall x in M) (M\ {x} is not transversal for S).

% not(exists x in M) not(...... )
% not(exists x in M)(M\ {x} is transversal for S)

gen_list_min_transversal(L, M) :-
    gen_list_transversal(L, M),
    not((member(X, M),
    append(P, [X | R], M),
    append(P, R, U),
    is_list_transversal(L, U))).

 
list_length([], 0).
list_length([H | T], N) :-list_length(T, M), N #= M + 1. 

gen_length_of_min_list_transversal(L, K) :-
    gen_list_min_transversal(L, M),
    list_length(M, K).

% M is min transversal for S
% then if K is length of M, then there is no transversal with length K-1
% the above is not true!!

gen_minimal_lenght_of_transversal(L, L) :-
    list_length(L, N),
    between(1, N, K),
    gen_list_min_transversal(L, M),
    list_length(M, K),
    K1 #= K - 1,
    not((
       gen_list_min_transversal(L, T),
       list_length(T, K1) 
    )).


% gen_K_subsequnce(L, K, S)
% S is a subsequnce of L with length K

% gen_K_subsequnce(_, 0, []).
% gen_K_subsequnce(L, K, [H | T]) :-
%     K > 0,
%     member(H, L),
%     K1 is K - 1,
%     gen_K_subsequnce(L, K1, T).

gen_K_subsequnce(_, 0, []).
gen_K_subsequnce([_ | T], K, S) :-
    K > 0,
    gen_K_subsequnce(T, K, S).
gen_K_subsequnce([H | T], K, [H | S]) :-
    K > 0,
    K1 is K - 1,
    gen_K_subsequnce(T, K1, S).