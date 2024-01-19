:- use_module(library(clpfd)).


nat(0).
nat(N) :- nat(K), N#= K + 1.

between1(A, B, A): - A #=< B.
between1(A, B, R) :- A #< B, A1 #= A + 1, between(A1, B, R).

gen_KS(1, S, [S]).
gen_KS(K, S, [H | T]) :-
    K #> 0,
    between1(0, S, H ),
    NS #= S - H,
    K1 #= K - 1,
    gen_KS(K1, NS, T).

gen_pair(A, B) :-
        nat(N),
        gen_KS(2, N, [A, B]).


gen_arr_prog([]).
gen_arr_prog(P) :-
        nat(N),
        gen_KS(3, N, [L, A0, D]), % искам 3 числа със сума N, това да са дължината L, началното А0 и разликата Д
        L #> 0,
        gen_ar_prog_rec(L, A0, D, P). % генерирай ар. прогресия с д-на L, начално число A0, стъпка Д и това да е P.
    
gen_ar_prog_rec(1, A0, 0, [A0]).
gen_ar_prog_rec(L, A0, D, [A0 | P]) :-
    L #> 1,
    L1 #= L - 1,
    A1 #= A0 + D,
    gen_ar_prog_rec(L1, A1, D, P).  
    



% Ще направим прогресията да бъде за целите числа
% Дефинираме предикат
int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= - N.

% Генерира всевъзможните прогресии от цели числа
gen_arr_prog_Z([]).
gen_arr_prog_Z(P) :-
    nat(N),
    gen_KS(3, N, [L, K, M]),
    L #> 0, 
    int(K, A0),
    int(M, D),
    gen_ar_prog_rec(L, A0, D, P).