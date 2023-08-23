:- use_module(library(clpfd)).

% works for nested lists also
list_len([], 0).
list_len([_| T], N)  :- list_len(T, K), N #= K + 1.

% check whether a smt is a list
list([]).
list([ _ | T]) :- list(T).

% check whether X is element of L
member(X, [X | _]).
member(X, [Y | T]) :- member(X, T).

% check whether X is element of any of the lists in  L
member_in_list(X, [Sublist | R]) :-
    member(X, Sublist).

member_in_list(X, [_|Rest]) :-
    member_in_list(X, Rest).

first(Sublist, [Sublist | _]).

last(Sublist, [Sublist]).
last(Lsl, [ _ | Rest]) :-
    last(Lsl, Rest).


append([], L2, L2).
append([H | T], L2, [H | Result]) :- append(T, L2, Result).

reverse([], []).
reverse(Result, [H | T]) :- reverse(Temp, T), append(Temp, [H], Result).


%remove_all(X, L, R) - remove all occurences of X in L
remove_all(_, [], []).
remove_all(X, [X | T],R) :- remove_all(X, T, R).
remove_all(X, [Y | T], [Y | R ]) :- remove_all(X, T, R), X\=Y.

remove_all_from_ll(X, [], []).
remove_all_from_ll(X, [H | T], [R | Result]) :-
    remove_all(X, H, R),
    remove_all_from_ll(X, T, Result).

insert(X, L, R) :- append(P, S, L), append(P, [X | S], R).
    
%insert an element in all places in the list of lists.
insert_in_all_ll(X, [], [[X]]).
insert_in_all_ll(X, [H | T], [R | Result]) :-
    insert(X, H, R),
    insert_in_all_ll(X, T, Result).

% works for lists and lists of lists
pre_last(P, [P, T]).
pre_last(P, [_| T]):- pre_last(P, T).

second(S, [H, S | _]).

odd_position(X, [_, X | _]).
odd_position(X, [H, Y | T]) :- odd_position(X, T).

even_position(X, [X|_]).
even_position(X, [_ | T]) :- odd_position(X, T).


% works for nested lists also
k_th_element(X, [X| _], 1).
k_th_element(R, [_ | T], K) :-
    K > 1,
    K1 #= K - 1,
    k_th_element(R, T, K1).


