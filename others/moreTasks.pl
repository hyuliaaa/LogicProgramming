:- use_module(library(clpfd)).

nat(0).
nat(N) :- nat(K), N #= K + 1.

range(A, B, []) :- A #> B.
range(A, B, [A | R]) :- A #=< B, A1 #= A + 1, range(A1, B, R).

between(A, B, X) :- range(A, B, L), member(X, L).

gen_KS(1, Sum, [Sum]).
gen_KS(K, Sum, [H | T]):-
    K #> 0,
    between(0, Sum, H),
    NewSum #= Sum - H,
    K1 #= K - 1,
    gen_KS(K1, NewSum, T).


gen_pair(A, B):-
    nat(N),
    gen_KS(2, N, [A, B]).


% генерира всички крайни редици от ест. числа, които са с ненулева дължина
gen_fin_seq_Nat(L) :-
    gen_pair(K, S),
    gen_KS(K, S, L).

% генерирайте всевъзможните крайни подмн-ва на ест. числа
%gen_fin_subset_Nat(S).


gen_fin_subset_Nat([]).
% за решението можем да използваме предикатът за генериране на крайна редица от ест. числа
% трябва да филтрираме редиците по такъв начин, че да получим мн-во(няма повторения на елементи, нямаме наредба на елементите)
% Всяко мн-во трябва да го има само веднъж
gen_fin_subset_Nat(S) :-
    gen_fin_seq_Nat(S), 
    encodes_subset(S).

%  ще позволяваме да генерираме списъци, където елементите са подредени и то в строго растящ ред
encodes_subset([]).
encodes_subset([_]).
encodes_subset([A, B | T ]) :-
    A #< B,
    encodes_subset([B | T]).

% да се генерират всевъзможните аритметични прогресии от ест. числа
gen_arr_prog([]).
gen_arr_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, A0, D]),
    L #> 0,
    gen_arr_prog_rec(L, A0, D, P).

gen_arr_prog_rec(1, A0, _, [A0]).
gen_arr_prog_rec(L, A0, D, [A0 | R]) :-
        L #> 1,
        L1 #= L - 1,
        A1 #= A0 + D,
        gen_arr_prog_rec(L1, A1, D, R).


% По дадено число А, генерираме списъци [X,Y] [Z, U], които кодират рационално число.
% q(A, [X, Y], [Z,U]) -> given a natural number A
% generate [X, Y] [Z, U] -> which represent a rational numbers
% such that Y > X > 0, Z > U > 0
% and (X/Y) * (Z/U) = 2
% X + Z < A


q(A, [X, Y], [Z, U]):-
    gen_two_positive_rat_num([X, Y], [Z, U]),
    X + Z #< A,
    X * Z #= 2 * Y * U.


gen_4_touples_nats(A, B, C, D) :-
    nat(N),
    gen_KS(4, N, [A, B, C, D]).


gen_two_positive_rat_num([X, Y], [Z, U]):-
    gen_4_touples_nats(X, Y, Z, U),
    X #> 0, Y #> 0, Z #>0, U #> 0,
    gcd(X, Y, 1),
    gcd(Z, U, 1).

gcd(0, B, B).
gcd(A, B, D) :-
    A > 0,
    R #= B mod A,
    gcd(R, A, D).

% генерирайте двойки в декартовата равнина, които са с целочислени координати и са в кръг
% gen_in_circle([XC, YC], R, [X, Y]).
% given integers XC and YC and R, R is positive
% generate integers X Y, such that
% [X, Y] is point inside the cicle with center (XC, YC) and radius R

gen_in_square(R, A, B) :-
    MR #= - R,
    between(MR, R, A),
    between(MR, R, B).

gen_in_circle_00(R, A, B) :-
    gen_in_square(R, A, B),
    A^2 + B^2 #=< R^2.


gen_in_circle([XC, YC], R, [X, Y]):-
    gen_in_circle_00(R, A, B),
    X #= A + XC,
    Y #= B + YC.