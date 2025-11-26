:- [tp_lmc].  % charger ton code d'unification

% Fonction utilitaire pour tester et afficher le résultat
test_unification(NomTest, Systeme, Strategie, Mode, ResultatAttendu) :-
    write('=== Test: '), write(NomTest), write(' '), write(Mode), write(', Strat: '), write(Strategie), write(' ==='), nl,
    (   catch(call(Mode, Systeme, Strategie), _, fail) ->
        Resultat = true
    ;   Resultat = false
    ),
    ( Resultat == ResultatAttendu ->
        write('Succès!'), nl
    ;   write('Échec! Résultat attendu: '), write(ResultatAttendu), nl
    ),
    nl.

% Liste de tests
run_all_tests :-
    % Tests simples
    test_unification('Test1', [X ?= a, Y ?= X], choix_premier, unif, true),
    test_unification('Test1_bis', [X ?= a, Y ?= X], choix_premier, trace_unif, true),
    test_unification('Test2', [X ?= a, Y ?= X], choix_pondere_1, unif, true),
    test_unification('Test2_bis', [X ?= a, Y ?= X], choix_pondere_1, trace_unif, true),
    test_unification('Test3', [X ?= a, Y ?= X], choix_pondere_2, unif, true),
    test_unification('Test3_bis', [X ?= a, Y ?= X], choix_pondere_2, trace_unif, true),
    test_unification('Test4', [X ?= a, Y ?= X], choix_random, unif, true),
    test_unification('Test4_bis', [X ?= a, Y ?= X], choix_random, trace_unif, true),

    % Tests composés
    test_unification('Test5', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_premier, unif, true),
    test_unification('Test5_bis', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_premier, trace_unif, true),
    test_unification('Test6', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_1, unif, true),
    test_unification('Test6_bis', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_1, trace_unif, true),
    test_unification('Test7', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_2, unif, true),
    test_unification('Test7_bis', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_2, trace_unif, true),
    test_unification('Test8', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_random, unif, true),
    test_unification('Test8_bis', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_random, trace_unif, true).

