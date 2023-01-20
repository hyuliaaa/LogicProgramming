:- use_module(library(clpfd)).


nat(0).
nat(X) :- nat(N), X #= N + 1.

between1(A, B, A):-A#=<B.
between1(A, B,  R) :- A#<B, A1#=A+1, between(A1, B, R).

% generate list of K numbers with sum S gen_KS(K,S,L)
gen_KS(1, S, [S]).
gen_KS(K, S, [H | T ]) :-  
    K #> 0,
    between1(0, S, H), % generate number between 0 and S called H
    N #= S - H,
    K1 #= K - 1, 
    gen_KS(K1, N, T).


gen_pair(A, B) :-
    nat(N),
    gen_KS(2, N, [A, B]).


% Обединението на N на к-та
% gen_union_N_K(L) -> L е обединението по ест числа k в N^k
% L e списък от к на брой ест. числа

% генерира всички крайни редици от ест. числа, които са с ненулева дължина
gen_fin_seq_Nat(L) :- % Канторово изброяване
    gen_pair(K, S),
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

% какво трябва да не е вярно, че да е вярно, че списъкът е строго растящ
encodes_subset_not(L) :-
        not((append(_, [A, B | _], L), A#>=B)).
         
% да се генерират всевъзможните аритметични прогресии от ест. числа
%gen_arr_prog(P).
gen_arr_prog([]).
gen_arr_prog(P) :-
    nat(N),
    gen_KS(3, N, [L, A0, D]), % искам 3 числа със сума N, това да са дължината L, началното А0 и разликата Д
    L #> 0,
    gen_ar_prog(L, A0, D, P). % генерирай ар. прогресия с д-на L, начално число A0, стъпка Д и това да е P.

gen_ar_prog(1, A0, 0, [A0]).
gen_ar_prog(L, A0, D, P) :-
    L #> 1,
    gen_ar_prog_rec(L, A0, D, P).

gen_ar_prog_rec(1, A0, _, [A0]).
gen_ar_prog_rec(L, A0, D, [A0 | P]) :-
    L #> 1,
    L1 #= L - 1,
    A1 #= A0 + D,
    gen_ar_prog_rec(L1, A1, D, P).

% Ще направим прогресията да бъде за целите числа
% Дефинираме предикат
% int(N, Z) -> if { n == 0, then z=0 else Z is in {N, -N} 
int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.


%nat(N), int(N,Z) - генерирай ест, число N, след това  по N генерирай цяло число 

% Генерира всевъзможните прогресии от цели числа
gen_arr_prog_Z([]).
gen_arr_prog_Z(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L #> 0,
    int(K, A0),
    int(M, D),
    gen_ar_prog(L, A0, D, P). % генерирай ар. прогресия с д-на L, начално число A0, стъпка Д и това да е P.

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
    Y#>X, Z#> U,
    X + Z < A,
    X * Z #= 2 * Y * U.

gcd(0, B, B).
gcd(A, B, D) :-
    A > 0,
    R is B mod A,
    gcd(R, A, D).


% генериране на всевъзможни четворки
gen_4_tuples_nats(A, B, C, D) :-
    nat(N),
    gen_KS(4, N, [A, B, C, D]).

% Остава да генерираме съответните рационални числа
gen_two_positive_rat_numbers([X,Y], [Z,U]) :-
    gen_4_tuples_nats(X, Y, Z, Y).
    X #> 0, Y #> 0, Z #>0, U #> 0,
% трябва да проверим, че списъка от [X, Y] представялва рац. число и списъка от [Z, Y]
% филтрираме тези, които са взаимно прости
    gcd(X, Y, 1),
    gcd(Z,Y, 1).

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


% ще искаме да генерираме А, Б, която е в квадрата   -R<=A<=R, -R<=B<=R
gen_in_square(R, A, B) :-  %-R<=A<=R, -R<=B<=R [генерираме всички точки А, Б в квадрата]
    MR #= -R,
    between(MR, R, A), 
    between(MR, R, B).

gen_in_circle_00(R, A, B) :- % приемаме радиус R и генерираме A и В 
    gen_in_square(R, A, B),
    A^2 + B^2 #=< R^2.

gen_in_circle1([XC, YC], R, [X,Y]) :-
    gen_in_circle_00(R, A, B),
    X #= A + XC,
    Y #= B + YC.

% solution 2: (X-XC)^2 + (Y - YX)^2 <=R^2


% gen_int_closed(L, M) : given list of integers L generate M such that SET(M) is subset of SET(L)
% тоест всеки елемент на М е елемент на L
% M represents Set
% M is integer closed: (каквото и а да взема от М)( и съществува б от L), то е вярно че ({a+b, a-b, a*b} e подм-во на L)

% пример: L = [1,2,0], M = [0], M = [1,2]

gen_int_closed(L, R) :-
    distinct_elements(L, S),
    subset(S, M),
    is_integer_closed_with_rec(M, L).

subset([], []).
subset([H | T], [H | S]) :- subset(T, S).
subset([H | T], S) :- subset(T, S).


distinct_elements([], []).
distinct_elements([H | T], R) :-
     distinct_elements(T, D),
     insert_distinct(H, D, R).

insert_distinct(E, D, D) :- member(E, D).  % if D are distinct and E is member, dont add E)
insert_distinct(E, D, [E | D]) :- not(member(E, D)).

is_integer_closed_with_rec([], L).
is_integer_closed_with_rec([H | T ], L) :-
    is_integer_closed_with_rec(T, L),
    is_element_closed(H,M, L).

is_element_closed(E, M, L) :-
        member(X, M),
        A #= E + X,
        B #= E - X,
        C #= E * X,
        member(A, L),
        member(B, L),
        member(C, L).
    