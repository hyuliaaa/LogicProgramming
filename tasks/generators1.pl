:- use_module(library(clpfd)).

nat(0).
nat(N) :- nat(K), N#= K + 1.

between(A, B, A) :- A #=< B.
between(A, B, X) :- A #< B, A1 #= A + 1, between(A1, B, X).

% generate list of K numbers with sum S gen_KS(K,S,L)
gen_KS(1, S, [S]).
gen_KS(K, S, [H | T]):-
    K #> 0,
    between(0, S, H),
    NS #= S - H,
    K1 #= K - 1,
    gen_KS(K1, NS, T).

gen_pair(A, B) :-
        nat(N),
        gen_KS(2, N, [A,B]).

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
encodes_subset([A, B | T]) :-
    A #<B,
    encodes_subset([B | T]).

% да се генерират всевъзможните аритметични прогресии от ест. числа
%gen_arr_prog(P).
gen_arr_prog([]).
gen_arr_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, A0, D]),
    L #> 0,
    gen_arr_prog_rec(L, A0, D, P).

gen_arr_prog_rec(1, A0, 0, [A0]).
gen_arr_prog_rec(L, A0, D, [A0 | P]) :-
    L #>1,
    A1 #= A0 + D,
    L1 #= L - 1,
    gen_arr_prog_rec(L1, A1, D, P).

% Ще направим прогресията да бъде за целите числа
int(0, 0),
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.

% Генерира всевъзможните прогресии от цели числа
gen_arr_prog_Z([]).
gen_arr_prog_Z(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L #> 0,
    int(K, A0),
    int(M , D),
    gen_arr_prog_rec(L, A0, D, P).

% По дадено число А, генерираме списъци [X,Y] [Z, U], които кодират рационално число.
% q(A, [X, Y], [Z,U]) -> given a natural number A
% generate [X, Y] [Z, U] -> which represent a rational numbers
% such that Y > X > 0, Z > U > 0
% and (X/Y) * (Z/U) = 2
% X + Z < A

% catch 1: the cond (X/Y) * (Z/U) = 2 must be checked in integers
% X * Z = 2 * Y * U
% catch 2: when does [A,B] represent a rational number? (when A and B are цели numbers)     
% А е цяло число и В е естевено ненулево и GCD(A,B) = 1

q(A, [X, Y], [Z,U]) :-
    gen_two_positive_rat_numbers([X,Y], [Z,U]),
    Y #> X, Z #> U,
    X + A < A,
    X * Z #= 2*Y*U.

% Остава да генерираме съответните рационални числа
gen_two_positive_rat_numbers([X,Y], [Z, U]) :-
    get_4_tuples_nats(X,Y,Z, Y),
    X #> 0, Y #> 0, Z#>0, U#> 0,
% трябва да проверим, че списъка от [X, Y] представялва рац. число и списъка от [Z, Y]
% филтрираме тези, които са взаимно прости
    gcd(X, Y, 1),
    gcd(Z, Y, 1).

    gcd(0, B, B).
    gcd(A, B, D) :-
    A > 0,
    R #= B mod A,
    gcd(R, A, D).

get_4_tuples_nats(A, B, C, D) :-
    nat(N),
    gen_KS(4, N, [A, B, C, D]).

% генерирайте двойки в декартовата равнина, които са с целочислени координати и са в кръг
% gen_in_circle([XC, YC], R, [X, Y]).
% given integers XC and YC and R, R is positive
% generate integers X Y, such that
% [X, Y] is point inside the cicle with center (XC, YC) and radius R

% solution 1: we generate [A,B] inside cicle with center (0, 0) and radius R  
% we shift: (X,Y) = (A + XC, B + YC )
% -R<=A<=R, -R<=B<=R (А и Б всъщност могат да се намират в квадрат с д-на на страната 2R
% и центъра е (0,0 )) 
% (A, B) : A^2 + B^2 <=R2 [we check if a point is in a circle with center (0,0)]

example_labeling(X) :-
    X in 1..3,
    label([X]).


perm(N, P) :- length(P, N), P ins 1..N, all_distinct(P), label(P).


% специфичен(X) :- 
%     дължина(X, 3*N), 
%     forall( 
%                 ( K in 1..N, label([K]) ),
% 	            ( nth(X, K, Ak),
% 	            nth(X, 3*K, A3k),
% 	            (  1 #=< Ak+6, Ak+6 #=< 3*N, 1 #=< Ak, Ak #=< K
% 	            -> nth(X, Ak+6, A3k); 
%              A3k #= 3*A3nk+1, nth(X, 3*N-K, A3nk) ) )
%          ).
