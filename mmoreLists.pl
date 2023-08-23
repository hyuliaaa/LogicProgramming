:- use_module(library(clpfd)).


append([], L2, L2).
append([H | T], L2, [H | R]):- append(T, L2, R). 

% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=<B.
min(B, A, B) :- A #> B.

% min(L, M) -> M is the minimum element of L(list)
min_el([M], M).
min_el([H | T], M):- min_el(T, K), min(M, H, K).

list_len([], 0).
list_len([_| T], N):-  list_len(T, K), N #= K + 1.

% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X | _], 0).
element_at(X, [_ | T], N) :- N #> 0, K #= N - 1, element_at(X, T, K).

is_sorted([]).
is_sorted([_]).
is_sorted([X, Y | T]) :- X #=< Y, is_sorted([Y | T]).

bogo_sort(L, S) :- permutate(L, S), is_sorted(S).

permutate([], []).
permutate([H | T], R) :- permutate(T, Curr), insert(H, Curr, R).

insert(X, L, R):- append(P, S, L), append(P, [X | S], R).

% poluchavash element i spisyk ot spisaci i kym vseki spicyk dobavash tozi element
insert_first(_, [], []).
insert_first(X, [L | LS], [[X | L] | RS]) :- insert_first(X, LS, RS).

%between(X, A, B) :- checks/generates X(integer) is between A and B.
between(A, A, B) :- A #=<B.
between(X, A, B) :- A #< B, A1 #= A + 1, between(X, A1, B).

%range(L, A, B) :- L= [A, A+1...B]
range([], A, B) :- A #> B.
range([A | L], A, B) :- A #=< B, A1 #= A + 1, range(L, A1, B).

between_with_range(X, A, B) :- range(R, A, B), member(X, R).

% generate list with length k with elements from A to B
% list_of_K_elements_between_A_B(L, K, A, B)
list_of_K_elements_between_A_B([], 0, _, _).
list_of_K_elements_between_A_B( [H | T], K, A, B) :- K #> 0,
                                             N #= K - 1,
                                             between(H, A, B),
                                             list_of_K_elements_between_A_B(T, N, A, B).

% da postroim spisyk s dyljina k s elementi ot nqkakyv spisyk
%variations_with_repetiotions(V, K, L) -: V is list with length K and each element is element of L.
variations_with_repetitions([], 0, _).
variations_with_repetitions([H | T], K, L) :-
            K #> 0,
            N #= K - 1,
            member(H, L),
            variations_with_repetitions(T, N, L).


% gen_KS(K,S,L):-L is a list with length K and sum of elements in the list S. 
% списък от К числа със сума S.

gen_KS(1, Sum, [Sum]).
gen_KS(K, Sum, [H | T]):-
    K #> 0,
    between(0, Sum, H),
    N #= Sum - H,
    K1 #= K - 1,
    gen_KS(K1, N, T).


% Generators
nat(0).
nat(N) :- nat(K), N #= K + 1.

% gen_pair_of_nats(X, Y) :-nat(X), nat(Y).
gen_pair_of_nats(X, Y) :- nat(Sum),
                          between(0, Sum, X),
                          Y #= Sum - X.

gen_pair_of_nats2(X, Y) :-
      nat(Sum),
      gen_KS(2, Sum, [X, Y]).

% int(N, Z) -> if { n == 0, then z=0 else Z is in {N, -N} 
int(0, 0).
int(N, N) :- N #> 0.
int(N, Z) :- N #> 0, Z #= -N.

% генерирайте двойките цели числа Z^2
gen_pair_of_ints(A, B) :-
    gen_pair_of_nats(N, K),
    int(N, A),
    int(K, B).

% генерира всички крайни редици от ест. числа, които са с ненулева дължина
gen_fin_seq_Nat(L) :- % Канторово изброяване
    gen_pair_of_nats(K, S),
    gen_KS(K, S, L).

% връща истина или лъжа
% вярно ли е, че всички елементи на списъка са положителни (false)
forall(member(A, [2,4,6,-7]), A #> 0).

%вярно ли е, че всички елементи на списъка са четни
forall(member(A, [2,8,4]), A #= 2*K).

% подсигуряваме, че К ще има стойност
forall(member(A, [2,4,6,-8]), (A #= 2*K, label([K]))).

%  Да се дефинира предикат r(X,Y), който по дадени списъци X и Y
% проверява дали всеки елемент на X, който е четно число, е елемент на Y.
%r(X, Y) - всеки елемент на X, който е четно число е елемент на Y

r(X, Y) :- forall( (member(A, X), A #= 2*B, label([B])),
                    member(A, Y)).


% предикат за подмн-во (елементите на Х са елементи на У).
subset(X, Y) :- forall(member(A, X), member(A, Y)).


subset([], []).
subset([H | T], [H | S]) :- subset(T, S).
subset([H | T], S) :- subset(T, S).



% findall(A, P, X) е приблизително същото като X = {A: P}
% т.е. X става списък от всички A, за които P е вярно.
?- findall(A,(member(A,[1,3,4,2,9,22]), A #= 2*K), X).
X = [4, 2, 22].
