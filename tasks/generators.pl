:- use_module(library(clpfd)).

% nat(X) -> generates X in N(natural numbers)
nat(0).
nat(N) :- nat(K), N #= K + 1.


% iskame da generirame dekartoviq kvadrat(декартово произведение) na N
%Praviloto e takova: posledniq bezkraen generator pobejdava.
% Zatova, ako polzvame bezkraen generator praktichecskoto pravilo e slednoto:
% Pyrvo prilagame bezkrainiq generator, sled koeto prilagame kraini generatori

% ideqta e po nqkakyv nachin da namerim sumata i da ogranichim dvete otgore

between(A, B, A) :- A #=<B.
between(A, B, X) :- A #< B, A1 #= A + 1, between(A1, B, X).

gen_pair_of_nats(X, Y) :- nat(Sum),
                          between(0, Sum, X),
                          Y #= Sum - X.


 % генерирайте двойките цели числа Z^2
 gen_pair_of_ints(A, B) :-
    gen_pair_of_nats(N, K),
    int(N, A),
    int(K, B).

int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N. 




