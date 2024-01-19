:- use_module(library(clpfd)).

length([], 0).
length([H | T], N) :- lenght(T, K), N #= K + 1, N #> 0.

sum([], 0).
sum([H | T], S) :- sum(T, Temp), S #= Temp + H.

% nth(A, N, L) - A е N-тият елемент на списъка L
nth(A, 1, [A | _]).
nth(A, N, [H | T]) :- N #>= 0, N1 #= N - 1, nth(A, N1, T).


% A in 5..13 <=> A #>= 5, A #=< 13
% [A,B,C] ins 5..13 <=> A in 5..13, B in 5..13, C in 5..13.

% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=< B.
min(B, A, B) :- A #>= B.

% min(M, L) -> M is the minimum element of L(list)

min_el(M, [M]).
min_el(M, [H | T]) :- min_el(K, T), min(M, H, K).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

permutate([], []).
permutate([H | T], P) :- permutate(T, Q), insert(H, Q, P).

bogo_sort(L, S) :- permutate(L, S), is_sorted(S).


%range(L, A, B) :- L= [A, A+1...B]
range([], A, B) :- A #> B.
range([A | L], A, B) :- A #=< B, A1 #= A + 1, range(L, A1, B).

%between(X, A, B) :- checks/generates X(integer) is between A and B.
between_with_range(X, A, B) :- range(R, A, B), member(X, R).

 % ограничен_списък(N, X) - X е списък с дължина не повече от N,
%                          чиито елементи са естествени числа, не
%                          по-големи от N

limited_list(N, X) :-
    K #=< N,
    length(X, K),
    X ins 0..N.

% факториел(N, F) - F = N!

fact(0, 1).
fact(N, F) :- N >= 0,
 N1 #= N - 1,
  F#= Res * N, 
  fact(N1, Res).

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)

list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B([H | L], K, A, B) :- K#>0, N #= K -1,
                                                between_with_range(H, A, B),
                                                list_of_K_elements_between_A_B(L, N, A, B).