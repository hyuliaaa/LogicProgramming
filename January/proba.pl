:- use_module(library(clpfd)).

% 5 ?- N #> 2, N #< 10, label([N]).
% N = 3 ;
% N = 4 ;
% N = 5 ;
% N = 6 ;
% N = 7 ;
% N = 8 ;
% N = 9.

nat(0).
nat(X) :- nat(N), X #= N + 1.

length([], 0).
length([_| T], N) :-length(T, K), N#= K + 1.


% генератор на двойките естествени числа

pair(X, Y) :- nat(N), X #< N, Y #<N, X #> 0, Y #> 0, label([X, Y]). 

int(K) :- nat(N), ( K #= N ; K #= -N), label([K]).

% generate all lists of natural numbers
list_natural(L) :-
    nat(N),
    Len in 0..N,
    length(L, Len),
    L ins 0..N,
    label(L).

        
    