
:- use_module(library(clpfd)).

% nth(A, N, L) - A е N-тият елемент на списъка L
nth(A,  1, [A | _]).
nth(A, N, [H | T]) :- N1 #= N - 1, nth(A, N1, T).

length([], 0).
length([_| T], N):- length(T, K), N#= K + 1.

append([], L2, L2).
append([H | T], L2, [H | R]) :- append(T, L2, R).

reverse([], []).
reverse([H | T], R) :- reverse(T, Temp), append(Temp, [H], R).

%Eliminate consecutive duplicates of list elements.
?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
X = [a,b,c,a,d,e]

compress([], []).
compress([X], [X]).
compress([X,X| XS], ZS) :- compress([X | XS], ZS).
compress([X,Y|YS],[X|ZS]) :- X \= Y, compress([Y|YS],ZS).

%Pack consecutive duplicates of list elements into sublists.
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

pack([], []).
pack([X], [[X]]).
pack([X,X | Xs], [[X|Zs]|R]) :- pack([X|Xs], [Zs|R]).
pack([X,Y | XS], [[X] | R]) :- X\=Y, pack([Y | XS], R).


% encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

first([X | _], X).


encode(F, R) :- pack(F, L), transform(L, R).

transform([], []).
transform([X | XS],[[N, F] | R] ) :- length(X, N), 
                         first(X, F),
                         transform(XS, R).

% encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(F, R) :- pack(F, L), transform_modified(L, R).

transform_modified([], []).
transform_modified([X | XS], [E | R]) :- length(X, 1),
                                  first(X, E), 
                                  transform_modified(XS, R).

transform_modified([X| XS], [[N, E] | R]) :- length(X, N),
                         N > 1,
                         first(X, E),
                         transform_modified(XS, R).

%idea 2
encode_modified2(L1,L2) :- encode(L1,L), strip(L,L2).

strip([], []).
strip([[1, X | YS]], [X | ZS]) :-strip(YS, ZS).
strip([[N , X | YS]], [[N,X | ZS]]) :- N #> 1, strip(YS, ZS).

%Decode a run-length encoded list.
% [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

repeat([0, X], []).
repeat([N, E], [E | R]) :- N #> 0, N1 #= N - 1, repeat([N1, E], R).

decode([], []).
decode([X | XS], [L | R]) :- repeat(X, L), decode(XS, R).


cycle(0, E, []).
cycle(Count, E, [E | R]) :-
    Count #> 0,
    NC #= Count - 1,
    cycle(NC, E, R).


% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]
dupli([], []).
dupli([X | XS], [X, X | YS ]) :- dupli(XS, YS).

% Duplicate the elements of a list a given number of times.
% Example:
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]



dupli1([], _, []).
dupli1([H | T], N, Result) :- cycle(N, H, CurrList),
                                     dupli1(T, N, R),
                                     append(CurrList, R, Result).


% Drop every N'th element from a list.
% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
% X = [a,b,d,e,g,h,k]

drop([], _, []).
drop([H | T], 1, T).
drop([H | T], N, [H | R]) :- N#>1, N1#= N - 1, drop(T, N1, R).


% Split a list into two parts; the length of the first part is given.
% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
% L1 = [a,b,c]
% L2 = [d,e,f,g,h,i,k]

split(L, 0, [], L).
split([H | T], N, [H | R1], R2) :- N #> 0, N1 #= N - 1, split(T, N1, R1, R2).

% Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% X = [c,d,e,f,g]

slice([], _, _, []).
slice([H | _], 1, 1, [H]).
slice([H | T], 1, F, [H | R] ):- F > 1, F1 #= F - 1, slice(T, 1, F1, R).
slice([_ | T], S, F, R) :- S #> 1, F > S, S1 #= S - 1, F1 #= F - 1, slice(T, S1, F1, R).



% combination(K,L,C) :- C is a list of K distinct elements 
%  chosen from the list L
element_removal(X, [X | L], L).
element_removal(X, [_ | L], R) :- element_removal(X, L, R).

combination(0, _, []).
combination(K, L, [X | C]) :-
    K > 0,
    element_removal(X, L, Rest),
    K1 is K - 1,
    combination(K1, Rest, C).


%  ############  Arithmetic  ##############


 % Determine whether a given integer number is prime
% is_prime(2).
% is_prime(3).
% is_prime(N) :- N > 3,
%                N mod 2 =\= 0,
%                not(has_divisor(N, 3)). 


has_divisor(N, D) :- N mod D =:= 0.
has_divisor(N, D) :-
% D * D < N, % we need to check u[ to square root of n]
D2 is D + 2, % skip even numbers
has_divisor(N, D2).

is_prime(P) :- P #>= 2, not((K #>= 2, K #< P, P #= K * M, label([K, M]))).


gcd(0, B, B).
gcd(A, B, D) :-
    A #> 0,
    R #= B mod A,
    gcd(R, A, D).


primes_between(A, B, []):- A #> B.
primes_between(A, B, [A |R ]) :-
    is_prime(A),
    A #=< B,
    A1 #= A + 1,
    primes_between(A1, B, R).
primes_between(A, B,R ) :- not(is_prime(A)), A1 #= A + 1, primes_between(A1, B, R).

    

