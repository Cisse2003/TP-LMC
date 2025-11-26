:- [tp_lmc].  % charger ton code d'unification


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