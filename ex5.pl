:- use_module(library(clpfd)).

% generate list of K numbers with sum S gen_KS(K,S,L)
gen_KS(1, S, [S]).
gen_KS(K, S, [H | T ]) :-  
    K #> 0,
    between(0, S, H), % generate number between 0 and S called H
    N #= S - H,
    K1 #= K - 1, 
    gen_KS(K1, N, T).


% Обединението на N на к-та
% gen_union_N_K(L) -> L е обединението по ест числа k в N^k
% L e списък от к на брой ест. числа
gen_union_N_K(L) :-
    nat(N),
    gen_KS(2, N, [K|S]),
    gen_KS(K, S, L).

nat(0).
nat(X) :- nat(N), X #= N + 1.

