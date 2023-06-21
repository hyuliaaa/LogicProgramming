:- use_module(library(clpfd)).

 % Ползваме го да проверява дали сумата на L, списък от положителни числа, е N или да го генерира L по N.
sum([], N):- N #= 0.
sum([H|T], N):- N #> 0, H #> 0, M #= N - H, sum(T, M).



% sum([], 0).
% sum([H | T], N):- sum(T, M), N #= M + H, N #> 0, H #> 0.

example_labeling(X) :-
    sum(X, 6),
    label(X).