:- use_module(library(clpfd)).

% nat(X) -> generates X in N(natural numbers)
nat(0).
nat(N) :- nat(K), K #= N + 1.


gen_pair_of_nats(X, Y) :- nat(Sum),
                          between(0, Sum, X),
                          Y #= Sum - X.

% списък от К числа със сума S.
gen_KS(1, Sum, [Sum]).  
gen_KS(K, Sum, [H | T]):-
    K #> 0,
    between(0, Sum, H),
    N #= Sum - H,
    K1 #= K - 1,
    gen_KS(K1, N, T).


% Дефинираме предикат
% int(N, Z) -> if { n == 0, then z=0 else Z is in {N, -N} 
int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.

% генерирайте двойките цели числа Z^2
gen_pair_of_ints(A, B) :- 
    gen_pair_of_nats(N, K),
    int(N, A),
    int(K, B).

gen_pair(A, B) :-
    nat(N),
    gen_KS(2, N, [A, B]).

% да се генерират всевъзможните аритметични прогресии от ест. числа
%gen_arr_prog(P).

gen_arr_prog([]).
gen_arr_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, A0, D]),
    L #> 0,
    gen_ar_prog_rec(L, A0, D, P).


gen_ar_prog_rec(1, A0, 0, [A0]).
gen_ar_prog_rec(L, A0, D, [A0 | P]) :-
    L #> 1,
    L1 #= L - 1,
    A1 #= A0 + D,
    gen_ar_prog_rec(L1, A1, D, P).

% Генерира всевъзможните прогресии от цели числа
gen_arr_prog_Z([]).
gen_arr_prog_Z(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L #> 0,
    int(K, A0),
    int(M, D),
    gen_ar_prog_rec(L, A0, D, P).


% По дадено число А, генерираме списъци [X,Y] [Z, U], които кодират рационално число.
% q(A, [X, Y], [Z,U]) -> given a natural number A
% generate [X, Y] [Z, U] -> which represent a rational number
% such that Y > X > 0, Z > U > 0
% and (X/Y) * (Z/U) = 2
% X + Z < A

% catch 1: the cond (X/Y) * (Z/U) = 2 must be checked in integers
% X * Z = 2 * Y * U
% catch 2: when does [A,B] represent a rational number? (when A and B are цели numbers)     
% А е цяло число и В е естевено ненулево и GCD(A,B) = 1

q(A, [X, Y], [Z, U]) :-
    gen_two_positive_rat_numbers([X, Y], [Z, U]),
    Y #> X, Z#> Y,
    X + Z < A,
    X * Z #= 2 * Y * U.

gen_two_positive_rat_numbers([X, Y], [Z, U]) :-
    nat(N),
    gen_KS(4, N, [X, Y, Z, U]),
    X #> 0, Y #> 0, Z #> 0, U #>0,
    gcd(X, Y, 1),
    gcd(Z, Y, 1).


gcd(0, B, B).
gcd(A, B, D) :-
    A #> 0,
    R #= B mod A,
    gcd(R, A, D).

