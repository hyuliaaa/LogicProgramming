:- use_module(library(clpfd)).

nat(0).
nat(N) :- nat(K), N #= K + 1.

between(A, B, A) :- A #=<B.
between(A, B, R) :- A #< B, A1 #= A + 1, between(A1, B, R).

range(A, B, []):- A #> B.
range(A, B, [A | R]):- A #=< B, A1 #= A + 1, range(A1, B, R).

gen_pair_of_nats(X, Y) :-
    nat(Sum),
    between(0, Sum, X),
    Y #= Sum - X.

gen_pair_of_nats2(X, Y) :-
    nat(Sum),
    gen_KS(2, Sum, [X,Y]).

% списък от К числа със сума S.
gen_KS(1, Sum, [Sum]).
gen_KS(K, Sum, [H | T]) :-
    K #> 0,
    between(0, Sum, H),
    NewSum #= Sum - H,
    K1 #= K - 1,
    gen_KS(K1, NewSum, T).


% генерирайте всички елементи на N^k
gen_Nat_K(K, T):-
    nat(Sum),
    gen_KS(K, Sum, T).

% Дефинираме предикат
% int(N, Z) -> if { n == 0, then z=0 else Z is in {N, -N} 
int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.


% генерирайте двойките цели числа Z^2
gen_pair_of_ints(A, B) :-
    gen_pair_of_nats(X, Y),
    int(X, A),
    int(Y, B).


% генерира всички крайни редици от ест. числа, които са с ненулева дължина
gen_fin_seq_Nat(L) :- % Канторово изброяване
    gen_pair_of_nats(K, S),
    gen_KS(K, S, L).


% генерирайте всевъзможните крайни подмн-ва на ест. числа
%gen_fin_subset_Nat(S).

gen_fin_subset_Nat([]). % празното мн-во за нас е празният списък
% за решението можем да използваме предикатът за генериране на крайна редица от ест. числа
% трябва да филтрираме редиците по такъв начин, че да получим мн-во(няма повторения на елементи, нямаме наредба на елементите)
% Всяко мн-во трябва да го има само веднъж

gen_fin_subset_Nat(S) :-
    gen_fin_seq_Nat(S), %позволяваме S да бъде редица от ест. числа
    % ще направим проверка дали кодира мн-во от ест. числа
    encodes_subset(S).

%  ще позволяваме да генерираме списъци, където елементите са подредени и то в строго растящ ред
encodes_subset([]).
encodes_subset([_]).
encodes_subset([A, B | T ]) :-
    A #< B,
    encodes_subset([B | T]).


% да се генерират всевъзможните аритметични прогресии от ест. числа
gen_arr_prog([]).
gen_arr_prog(P):-
    nat(N),
    gen_KS(3, N, [L, A0, D]),
    L #> 1,
    gen_arr_prog_rec(L, A0, D, P).


gen_arr_prog_rec(1, A0, _ , [A0]).
gen_arr_prog_rec(L, A0, D, [A0 | R]) :-
    L #> 1,
    L1 #= L - 1,
    A1 #= A0 + D,
    gen_arr_prog_rec(L1, A1, D, R).



% Генерира всевъзможните прогресии от цели числа
gen_arr_prog_Z([]).
gen_arr_prog_Z(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L #> 0,
    int(K, A0),
    int(M, D),
    gen_arr_prog_rec(L, A0, D, P).

gcd(0, B, B).
gcd(A, B, D) :-
    A #> 0,
    R #= B mod A,
    gcd(R, A, D).

gen_4_touples_nats(A, B, C, D) :-
    nat(N),
    gen_KS(4, N, [A, B, C, D]).


gen_two_positive_rat_num([X, Y], [Z, U]) :-
    gen_4_touples_nats(X, Y, Z, U),
    X #> 0,
    Y #> 0,
    Z #> 0,
    U #> 0,
    gcd(X, Y, 1),
    gcd(Z, U, 1).

q(A, [X, Y], [Z, U]) :-
    gen_two_positive_rat_num([X, Y], [Z, U]),
    Y #> X, Z #> U,
    X + Z #< A,
    X * Z #= 2 * Y * U.


% gen_in_circle([XC, YC], R, [X, Y])
% given integers XC, YC and R, R is positive
% generate integers X and Y such that
% (X, Y) is point inside the circle with center (XC, YC) and radius R.

% solution 1: generate (A, B) inside the circle with center (0, 0) and radius R.
% (X, Y) = (A + XC, B + YC)
% (A, B) : -R <= A <= R, -R <= B <= R, A^2 + B^2 <= R^2


gen_in_square(R, A, B) :-
    MR = -R,
    between(MR, R, A),
    between(MR, R, B).

gen_in_circle_00(R, A, B) :-
    gen_in_square(R, A, B),
    A^2 + B^2 =< R^2.

gen_in_circle([XC, YC], R, [X, Y]):-
    gen_in_circle_00(R, A, B),
    X is A + XC,
    Y is B + YC.

