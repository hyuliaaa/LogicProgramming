:- use_module(library(clpfd)).

between(A, B, A) :- A #=< B.
between(A, B, X) :- A #< B,A1 #= A + 1, between(A1, B, X).

range(A, B, []) :- A #> B.
range(A, B, [A | R]) :- A #=< B, A1 #= A + 1, range(A1, B, R). 

between_with_range(A, B, X) :- range(A, B, L), member(X, L).

% gen_K_with_sum_S(K,S,L):-L is a list with length K and sum of elements in the list S. 
gen_KS(1, Sum, [Sum]).
gen_KS(K, Sum, [H | T]) :-
    K #> 0,
    K1 #= K - 1,
    between(0, Sum, H),
    N #= Sum - H,
    gen_KS(K1, N, T).


nat(0).
nat(N) :- nat(K), N #= K + 1.

% gen_pair_of_nats(X, Y)
gen_pair_of_nats(X, Y) :-
    nat(Sum),
    between(0, Sum, X),
    Y #= Sum - X.


% генерирайте всички елементи на N^k
gen_Nat_K(K, T) :-
    nat(Sum),
    gen_KS(K, Sum, T).

% генерирайте обединението на N^K -> N U N x N U N x N x N U ...
gen_union_Nat_K(T) :-
    gen_pair_of_nats(K, S),
    K #> 0,
    gen_KS(K, S, T).



gen_pair_of_ints(A, B) :-
    gen_pair_of_nats(X, Y),
    int(X, A),
    int(Y, B).


int(0, 0).
int(N, N):- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.

