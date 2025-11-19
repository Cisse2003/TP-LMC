:- op(20,xfy,?=).

% Prédicats d'affichage fournis
set_echo :- assert(echo_on).
clr_echo :- retractall(echo_on).
echo(T) :- clause(echo_on, _), !, write(T).
echo(_).

% Teste si la variable V apparaît dans le terme T
occur_check(V, T) :- V == T,!.
occur_check(V, T) :- var(V),nonvar(T), sub_term(Z, T), Z == V,!.

% Détermine la règle de transformation R qui s'applique à l'équation E
regle(S ?= T, rename) :- var(S), var(T), S \== T.
regle(S ?= S, delete) :- var(S).
regle(S ?= T, simplify) :- atomic(T), var(S), S \== T.  % CORRIGÉ ICI
regle(S ?= T, expand) :- var(S), compound(T), \+ occur_check(S, T).
regle(T ?= X, orient) :- var(X), \+ var(T).
regle(S ?= T, decompose) :- compound(S), compound(T), functor(S, F, N), functor(T, F, N).
regle(S ?= T, clash) :- compound(S), compound(T), functor(S, F1, N1), functor(T, F2, N2), ( F1 \== F2 ; N1 \== N2 ).
regle(S ?= T, check) :- var(S), S \= T, occur_check(S, T).

% Transforme le système d'équations
reduit(delete, _ ?= _, P, P).
reduit(rename, S ?= T, P, Q) :- subst(S,T,P,Q).
reduit(simplify, S ?= T, P, Q) :- subst(S,T,P,Q).
reduit(expand, S ?= T, P, Q) :- subst(S, T, P, Q).
reduit(orient, T ?= S, P,[S ?= T | P]).
reduit(decompose, S ?= T, P, Q) :-
    S =.. [_|ArgsS],
    T =.. [_|ArgsT],
    pairs_to_eqs(ArgsS, ArgsT, EqList),
    append(EqList, P, Q).
reduit(clash, _, _, _) :-
    write('Erreur : clash entre fonctions'), nl,
    fail.
reduit(check, _, _, _) :-
    write('Erreur : occur check échoué'), nl,
    fail.

% Substitutions
term_subst(X, T, InTerm, OutTerm) :-
    InTerm == X, !,
    OutTerm = T.
term_subst(_, _, InTerm, InTerm) :-
    atomic(InTerm), !.
term_subst(_, _, InTerm, InTerm) :-
    var(InTerm), !.
term_subst(X, T, InTerm, OutTerm) :-
    InTerm =.. [F|Args],
    maplist(term_subst(X, T), Args, Args2),
    OutTerm =.. [F|Args2].

subst(_, _, [], []).
subst(X, T, [H|Tail], [H2|Tail2]) :-
    H =.. [Op, A, B],
    term_subst(X, T, A, A2),
    term_subst(X, T, B, B2),
    H2 =.. [Op, A2, B2],
    subst(X, T, Tail, Tail2).

pairs_to_eqs([], [], []).
pairs_to_eqs([H1|T1], [H2|T2], [H1 ?= H2|EqT]) :-
    pairs_to_eqs(T1, T2, EqT).

unifie(P) :- set_echo, unifie(P, []).

unifie([],Q) :-
    echo(Q), nl,
    echo('Yes').

unifie(P,Q) :-
    echo('system: '), echo(P), nl,
    select(E, P, Rest),  % Utiliser select au lieu de [E|Rest]
    (regle(E, R) ->
     echo(R), echo(': '), echo(E), nl,
     reduit(R, E, Rest, S),
     unifie(S,Q)
    ;
     echo('No rule for: '), echo(E), nl, fail
    ).

% Tests
run_tests :-
    clr_echo,
    writeln('=== TESTS UNIFICATION ==='), nl,

    test1,
    test2,
    test3,
    test4,
    test5,

    writeln('=== TOUS LES TESTS REUSSIS ===').

test1 :-
    writeln('--- Test 1: X = a, Y = b ---'),
    set_echo,
    unifie([X ?= a, Y ?= b]),
    clr_echo, nl.

test2 :-
    writeln('--- Test 2: f(X,Y) = f(a,b) ---'),
    set_echo,
    unifie([f(X,Y) ?= f(a,b)]),
    clr_echo, nl.

test3 :-
    writeln('--- Test 3: f(X,Y) = f(g(Z),h(a)), Z = f(Y) ---'),
    set_echo,
    unifie([f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)]),
    clr_echo, nl.

test4 :-
    writeln('--- Test 4: f(X) = g(Y) (doit échouer) ---'),
    set_echo,
    (unifie([f(X) ?= g(Y)]) ->
        writeln('ERREUR: aurait dû échouer')
    ;
        writeln('OK: échec comme prévu')
    ),
    clr_echo, nl.

test5 :-
    writeln('--- Test 5: X = f(X) (occur check) ---'),
    set_echo,
    (unifie([X ?= f(X)]) ->
        writeln('ERREUR: occur check aurait dû échouer')
    ;
        writeln('OK: occur check fonctionne')
    ),
    clr_echo, nl.