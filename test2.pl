:- [tp_lmc].  % charger ton code d'unification

% Fonction utilitaire pour tester et afficher le résultat
test_unification(NomTest, Systeme, Strategie, ResultatAttendu) :-
    write('=== Test: '), write(NomTest), write(' ==='), nl,
    (   catch(unifie(Systeme, Strategie), _, fail) ->
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
    test_unification('Test1', [X ?= a, Y ?= X], choix_premier, true),
    test_unification('Test2', [X ?= a, Y ?= X], choix_pondere_1, true),
    test_unification('Test3', [X ?= a, Y ?= X], choix_pondere_2, true),
    test_unification('Test4', [X ?= a, Y ?= X], choix_random, true),

    % Tests composés
    test_unification('Test5', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_premier, true),
    test_unification('Test6', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_1, true),
    test_unification('Test7', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_pondere_2, true),
    test_unification('Test8', [f(X,Y) ?= f(g(Z),h(a)), Z ?= f(Y)], choix_random, true).
