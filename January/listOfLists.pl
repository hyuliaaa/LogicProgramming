:- use_module(library(clpfd)).

length([], 0).
lenght([H | T], N) :- lenght(T, K), N #= K + 1.

list([]).
list([_|T]) :-list(T).

% check whether X is element of L
member(X, [X | _]).
member(X, [Y | T]) :- member(X, T).

% check whether X is element of any of the lists in  L
member_in_list(X, [Sublist | Rest]) :- member(X, Sublist).
member_in_list(X, [_ | Rest]) :- member_in_list(X, Rest).

first(Sublist, [Sublist | _]).

last(X, [X]).
last(X, [ _ | R]) :- last(X, R).


append([], L2, L2).
append([H | T], L2,[H | R]) :- append(T, L2, R).


reverse([], []).
reverse([H | T], R ) :- reverse(T, Curr), append(Curr, [H], R).


reverse_also_elements([], []).
reverse_also_elements([H | T], [RevH | R] ) :- reverse(H, RevH), 
                                     reverse_also_elements(T, R).

% 21 ?- reverse_also_elements([[1,2], [3,4]], X).
% X = [[2, 1], [4, 3]].


reverse_list_of_lists([], []).
reverse_list_of_lists([H|T], Reversed) :-
    reverse_list_of_lists(T, ReversedT),
    reverse(H, RevH),
    append(ReversedT, [RevH], Reversed).

% 28 ?- reverse_list_of_lists([[1,2], [3,4]], Result).
% Result = [[4, 3], [2, 1]].

%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T],R):- remove_all(X, T, R).
remove_all(X, [Y | T], [Y | R]) :- remove_all(X, T, R), X \=Y.


remove_all_from_ll(X, [], []).
remove_all_from_ll(X, [H | T], [R | Res]) :-
    remove_all(X, H, R),
    remove_all_from_ll(X, T, Res).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).

%insert an element in all places in the list of lists.
insert_in_all_ll(X, [], []).
insert_in_all_ll(X, [H | T], [R | Res]) :-
    insert(X, H, R),
    insert_in_all_ll(X, T, Res).


pre_last(P, [P, T]).
pre_last(P, [_|T]) :- pre_last(P, T).

pre_last_a(P, L) :- append(_, [P, _], L).

k_th_element(X, [X | _], 1).
k_th_element(R, [_| T], K) :- K #> 1, K1 #= K - 1, k_th_element(R, T, K1).

sum([], 0).
sum([H | T], S):- sum(T, Sum), S #= H  + Sum.

sum_list([], 0).
sum_list([H | T], F) :-
    sum(H, S), 
    sum_list(T, R), 
    F #= S + R.
 
flatten([], []).
flatten([H | T], R) :- flatten(T, Temp), append(H, Temp, R).
 

