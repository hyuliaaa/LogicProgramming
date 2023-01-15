:- use_module(library(clpfd)).

% nat(X) -> generates X in N(natural numbers)
nat(0).
nat(N) :- nat(K), N#=K+1.

% gen_pair_of_nats(X, Y) :-nat(X), nat(Y).
% bezkraen generator, zashoto generira 0 za X i za Y {0,1,2.....inf}

% iskame da generirame dekartoviq kvadrat(декартово произведение) na N
%Praviloto e takova: posledniq bezkraen generator pobejdava.
% Zatova, ako polzvame bezkraen generator praktichecskoto pravilo e slednoto:
% Pyrvo prilagame bezkrainiq generator, sled koeto prilagame kraini generatori

% ideqta e po nqkakyv nachin da namerim sumata i da ogranichim dvete otgore

gen_pair_of_nats(X, Y) :- nat(Sum), %  give me some natural number S called Sum 
                          between(0, Sum, X), % generate number between 0 to sum called X
                          Y #=Sum-X.  

between(A, B, A):-A#=<B.
between(A, B,  R) :- A#<B, A1#=A+1, between(A1, B, R).


% генерирайте всички елементи на N^k
gen_Nat_K(K, T) :-
    nat(Sum),
    gen_KS(K, Sum, T).

% за 5 елемента ще бъде петорката от нули, после ще бъде 001

% списък от К числа със сума S.
gen_KS(1, Sum, [Sum]).  
gen_KS(K, Sum, [H | T]):-
    K #> 0,
    between(0, Sum, H),
    N #= Sum - H,
    K1 #= K - 1,
    gen_KS(K1, N, T).

% генерирайте обединението на N^K -> N U N x N U N x N x N U ...
gen_union_Nat_K(T) :-
    gen_pair_of_nats(K, Sum),
    K #> 0,
    gen_KS(K, Sum, T).

% генерирайте двойките цели числа Z^2

gen_pair_of_ints(A, B) :-
    gen_pair_of_nats(N, K), % N ни казва N-тото цяло число, К-тото цяло число
    int(N, A),
    int(K, B).

int(N, Z) :- N mod 2 #= 0, Z #= N div 2.
int(N,Z) :- N mod 2 #= 1, Z #= -((N -1) div 2) - 1. 
