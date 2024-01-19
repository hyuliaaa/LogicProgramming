:- use_module(library(clpfd)).

gcd(0, B, B).
gcd(A, B, D) :-
    A > 0,
    R is B mod A,
    gcd(R, A, D).


mutually_prime(A, B) :- gcd(A, B, 1).

find_even_elements_in_list([], []).
find_even_elements_in_list([H | T], [H | R]) :- 0 #= H mod 2, 
    find_even_elements_in_list(T, R).
find_even_elements_in_list([H | T],  R) :- 1 #= H mod 2, 
    find_even_elements_in_list(T, R).


k_th_element(X, [X| _], 1).
k_th_element(R, [_ | T], K) :-
    K > 1,
    K1 #= K - 1,
    k_th_element(R, T, K1).


% in nested lists finds the number of nested lists
elements_num([], 0).
elements_num([H | T], N) :- elements_num(T, X), N #= X + 1.

elements_sum([], 0).
elements_sum([H | T], N):- elements_sum(T, X), N #= X + H.


find_sum_nested([], 0).
find_sum_nested([H | T], R) :- elements_sum(H, N),
                              find_sum_nested(T, K),
                              R #= N + K.


% works for list of lists.
append([], L2, L2).
append([H | T], L2, [H | R]) :- append(T, L2, R).

% works for list pf lists
reverse([], []).
reverse([H | T],  R) :- reverse(T, Curr), append(Curr, [H], R).

% joins list of lists
join([], []).
join([L | LS], R) :- join(LS, T), append(L, T, R).

flatten(X, [X]):- not(is_list(X)).
flatten([], []).
flatten([H|T], R):- flatten(H, FH), flatten(T, FT), 
	append(FH, FT, R).


%Eliminate consecutive duplicates of list elements.
% compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [a,b,c,a,d,e]

compress([],[]).
compress([X],[X]).
compress([X,X|XS],ZS) :- compress([X|XS],ZS).
compress([X,Y|YS],[X|ZS]) :- X \= Y, compress([Y|YS],ZS).


% Pack consecutive duplicates of list elements into sublists.
%pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]

pack([], []).
pack([X], [[X]]).
pack([X,X | Xs], [[X|Zs]|R]) :- pack([X|Xs], [Zs|R]).
pack([X,Y | XS], [[X] |R]) :- X\=Y, pack([Y | XS], R).

% encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

find_element([X | _], X).

encode(F,R) :- pack(F,L), transform(L,R).

transform([],[]).
transform([X| XS], [[N, E] | R]) :- elements_num(X, N),
                                    find_element(X, E),
                                    transform(XS, R).

% encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(F, R) :- pack(F, L), transform_modified(L, R).
transform_modified([], []).
transform_modified([X | XS], [E | R]) :- elements_num(X, 1),
                         find_element(X, E),
                         transform_modified(XS, R).

transform_modified([X| XS], [[N, E] | R]) :- elements_num(X, N),
                         N > 1,
                         find_element(X, E),
                         transform_modified(XS, R).

%idea 2
encode_modified2(L1,L2) :- encode(L1,L), strip(L,L2).

strip([],[]).
strip([[1,X]|Ys],[X|Zs]) :- strip(Ys,Zs).
strip([[N,X]|Ys],[[N,X]|Zs]) :- N > 1, strip(Ys,Zs).

%Decode a run-length encoded list.
% [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

decode([], []).
decode([X| XS], [L | R]) :- repeat(X, L), decode(XS, R).

repeat([0, X], []). 
repeat([N, E], [E |R ]) :- N > 0, NN #= N - 1, repeat([NN, E], R).

flat_decode(L, R) :- decode(L, Temp), flatten(Temp, R).

cycle(0, E, []).
cycle(Count, E, [X | R]) :- 
    Count #> 0, 
    NewCount #= Count - 1, 
    cycle(NewCount, E, R).

% Duplicate the elements of a list.
% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).

% Duplicate the elements of a list a given number of times.
% Example:
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]

% dupli([], _, []).
% dupli([H | T], N, Result) :- cycle(N, H, CurrList),
%                                      dupli(T, N, R),
%                                      append(CurrList, R, Result).


% dupli([], _, Acc, Acc).
% dupli([H | T], N, Acc, FinalRes) :-
%         cycle(N, H, CurrList),
%         append(Acc, CurrList, NewAcc),
%         dupli(T, N, NewAcc, FinalRes).             


dupli(L1,N,L2) :- dupli(L1,N,L2,N).

% dupli(L1,N,L2,K) :- L2 is obtained from L1 by duplicating its leading
%    element K times, all other elements N times.
%    (list,integer,list,integer) (?,+,?,+)

dupli([],_,[],_).
dupli([_|XS],N,YS,0) :- dupli(XS,N,YS,N).
dupli([X|XS],N,[X|YS],K) :- K > 0, K1 is K - 1, dupli([X|XS],N,YS,K1).


% Drop every N'th element from a list.
% Example:
% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
% X = [a,b,d,e,g,h,k]

drop([], _, []).
drop([H | T], 1, T).
drop([H | T], N, [H | R]):- N > 1, NN #= N - 1, drop(T, NN, R).

% Split a list into two parts; the length of the first part is given.
% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
% L1 = [a,b,c]
% L2 = [d,e,f,g,h,i,k]

split(L, 0, [], L).
split([H | T], N, [H | R1], R2) :- N > 0, NN#= N - 1, split(T, NN, R1, R2).

% Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% X = [c,d,e,f,g]

slice([H | _], 1, 1, [H]).
slice([H | T], 1, F, [H | R]) :- F > 1, F1 #= F - 1, slice(T, 1, F1, R).
slice([_| T], S, F, R):- S > 1, S1 #= S - 1, F1 #= F - 1, slice(T, S1, F1, R). 


%Arithmetics
is_prime(2).
is_prime(3).
is_prime(N) :-N > 3,
              N mod 2 =\= 0, 
              not(has_divisor(N,3)).  

            
has_divisor(N, D) :- N mod D =:= 0.
has_divisor(N, D) :-
    D * D < N, % we need to check u[ to square root of n]
    D2 is D + 2, % skip even numbers
    has_divisor(N, D2).

% findall(A, P, X) е приблизително същото като X = {A: P}
% т.е. X става списък от всички A, за които P е вярно.
?- findall(A,(member(A,[1,3,4,2,9,22]), A #= 2*K), X).
X = [4, 2, 22].

c



% min(M, A, B) :- M is the minumum of A and B
min(A, A, B) :- A #=< B.
min(B, A, B) :- A #> B.


minEl([M], M).
minEl([H | T], M) :- minEl(T, CurrMin), min(M,H, CurrMin).

listLen([], 0).
listLen([H | T], N) :-listLen(T, K), N #= K + 1.


% element_at(X, L, N) :- whether X is in position N of list L
element_at(X, [X |_], 1).
element_at(X, [H | T], N) :- N #> 0, NN #= N - 1, element_at(X, T, NN).

%is_sorted(L) -: checks whether L is sorted
is_sorted([]).
is_sorted([_]).
is_sorted([X, Y | T]) :- X #=< Y, is_sorted([Y | T]).

% bogo_sort (L) :- generates permutations untill it finds one that is sorted

permutate([], []).
permutate([H | T], Result) :- permutate(T, Temp), insert(H, Temp, Result).