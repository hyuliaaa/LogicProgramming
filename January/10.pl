:- use_module(library(clpfd)).


%fibonacci

% f(AN - 1, AN) - 2 poredni elementa na red na fibonacci
% X, Y
% Y, X + Y = Z

f(0, 1). % 1-ва двойка елементи на редицата
f(Y, Z):- f(X, Y), Z#= X + Y.

% 0 1
% 1 1
% 1 2
% 2 3
% 3 5

% ot lqvo e redicata na fibonacci

% we will use this one as a generator
fibonacci(X):- f(X, _).


% всяка рекурентна редица може да я решаваме по този начин

% а0 = 1, а1 = 2, а3 = 2.
% an + 3 = an + 1 + 10*n - an + 0*an + 2

% an, an + 1, an + 2
% an + 1, an + 2, an + 1 + 10*n - an + 0*an + 2


% ============== Graphs ===============


% Графите: мн-во от върхове, мн-во от ребра
% неориентиран граф - мн-во от двойки
% оринтиран граф - наредени двойки

% V = [a, b, s, a, s, a, g........] 
% E = [[a,b], [d,s]......]


% искаме да проверим дали между U и V има ребро, занимаваме се с неориентирани графи
edge(U, V, E) :- member([U,V], E), member([V, U], E). % Може да са с или

% свързан - граф, за който между произволна двойка от върхове има път. (има път от всеки връх до всеки друг)

% (forall W)(forall U) [path(W, U)]

% - (exist W) (exists U) [- path(W, U)]  - не е вярно, че съществуват два върха между които няма път

isConnected([V, E]) :- not((member(W, V), member(U, V), W \= U, not(simplePath([V,E], W, U, P)))).


simplePath(_,_,_,_).


% прост път - всеки път между 2 върха, които не съдържа цикли (ацикличен - нямаме цикли в него!!)

%генерирайте дървета, за да можем да ги генерираме добре трябва да ги ограничим от нещо
% [], A, B  are trees

tree([], 0). % празното дърво има 0 върха
tree([A,B], N) :- N #>= 0, N #= M + K + 1, M #> 0, K #> 0, tree(A, M), tree(B, K).

tree(T):- nat(N), tree(T, N). % взимаме едно ест. число N и искаме да направим дърво, което има N върха.  

nat(0).
nat(N) :-nat(K), N #= K + 1.

% да се генерират всички списъци, чийто елементи са от даден списък (безкраен генератор)
% Имаме  списъка L и трябва да получим M : - елементите на М са елементи на L 

multiSubSet(_, []).
multiSubSet(L, [H | T]) :- member(H, L), multiSubSet(L, T).

% ?- multiSubSet([1,2,3], L)
     



% gen_prime(P) - generate all prime numbers

gen_prime(P) :-
    nat(P),
    is_prime(P).

is_prime(P) :-  P > 1,
                P1 #= P -1,
  not((
        between(2, P1, D), 
        P mod D =:= 0
    )).


% Q(i) -> count of prime numbers of kind 6k + 1 which are less than i
% check(X) <-> x = i + Q(i) for some positive natural i

% ще направим списък от всички прости числа, които са < i и са от вида 6k + 1

% К ще е резултатът
q(I, K) :- primes(I, PS), length(PS, K).

check(X) :-between(1, X, I), q(I, K), X #= I + K.

primes(I, PS) :- 
    I1 #= I - 1, 
    range(2, I1, L),
    filter(L, PS).

filter([], []).
filter([N | NS], T) :- not(condition(N)), filter(NS, T).
filter([N | NS], [N | T]) :- condition(N), filter(NS, T).

% ако  х е просто число и остатъка на х при деление на 6 е едно
condition(X) :- X mod 6 =:= 1, is_prime(X).

range(A, B, []) :- A #> B.
range(A, B, [A | T]) :- A #=< B, A1 #= A + 1, range(A1,B, T). 


% S are all elements from L, which are with numbers divisible by N and are between B and E
% slice(L, B, E, N, S)

slice([], _, _, _, []).
slice([H | T], B, E, N, [H | R]) :- H #>= B, H#=<B, 
                               index(H, [H | T], I), I mod N =:= 0, 
                               slice(T, B, E, N, R).
slice([H | T], B, E, N, R ) :- H #=< B; H #>=B;
index(H, [H | T], I), I mod N == 1,
slice(T, B, E, N, R). 
                            


index([X | _], X, 0).
index([H | T], X, N) :- N1 #= N - 1, H \= Y, index(T, X, N1).





