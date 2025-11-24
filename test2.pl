:- [tp_lmc].  % charger ton code d'unification

% Fonction utilitaire pour tester et afficher le résultat
test_unification(NomTest, Systeme,  ResultatAttendu) :-
write('=== Test: '), write(NomTest), write(' ==='), nl,
    ( catch(unif(Systeme, choix_premier), _, fail) -> R1 = true ; R1 = false ),
    ( catch(unif(Systeme, choix_pondere_1), _, fail) -> R2 = true ; R2 = false ),
    ( catch(unif(Systeme, choix_pondere_2), _, fail) -> R3 = true ; R3 = false ),
    ( catch(unif(Systeme, choix_random), _, fail) -> R4 = true ; R4 = false ),
    % on considère vrai si toutes les stratégies réussissent
    (R1, R2, R3, R4 -> Resultat = true ; Resultat = false),
    ( Resultat == ResultatAttendu ->
        write('Succès!'), nl
    ;   write('Échec! Résultat attendu: '), write(ResultatAttendu), nl
    ),
    nl.

% Liste de tests
run_all_tests :-
    % Tests simples
    test_unification('Test1', [X1 ?= a, Y1 ?= X1], true),

    % Tests composés
    test_unification('Test2', [f(X2,Y2) ?= f(g(Z2),h(a)), Z2 ?= f(Y2)], true),
    test_unification('Test3', [X3 ?= f(f(f(f(X3))))], false),
    test_unification('Test4', [X4 ?= f(g(h(X4, k(Y4)), Z4)), Y4 ?= f(X4)], false),
    test_unification('Test5', [X5 ?= f(Y5), Y5 ?= g(X5)], false),

    test_unification('Test6', [X6 ?= f(Y6), Y6 ?= g(Z6), Z6 ?= h(X6)], false),
    test_unification('Test7', [X7 ?= f(a, Y7), Y7 ?= f(b, Z7), Z7 ?= f(c, X7)], false),

    test_unification('Test8', [X8 ?= f(Y8, g(X8)), Y8 ?= h(X8, f(Y8))], false),
    test_unification('Test9', [X9 ?= f(g(X9, h(Y9))), Y9 ?= k(f(X9))], false),

    test_unification('Test10', [X10 ?= tree(L1, R1), Y10 ?= tree(L2, R2),
                                L1 ?= tree(a, b), L2 ?= tree(a, b),
                                R1 ?= tree(c, d), R2 ?= tree(c, d)], true),
    test_unification('Test11', [f(X11, g(Y11)) ?= f(a, g(b)), X11 ?= h(Z11), Y11 ?= h(W11)], false),
    test_unification('Test12', [X12 ?= f(Y12), Y12 ?= g(Z12), Z12 ?= a, X12 ?= W12], true),

    test_unification('Test13', [X13 ?= f(Y13,Z13), Y13 ?= g(a), Z13 ?= h(b), X13 ?= f(U13,V13)], true),
    test_unification('Test14', [f(X14, g(Y14)) ?= f(a, h(b)), X14 ?= Y14], false),
    test_unification('Test15', [X15 ?= f(a, b), X15 ?= f(Y15)], false),

    test_unification('Test16', [f(X16, a) ?= f(g(Y16), Y16), Y16 ?= h(X16)], false),
    test_unification('Test17', [f(X17,Y17) ?= g(X17,Y17)], false),
    test_unification('Test18', [f(X18) ?= f(X18,Y18)], false),
    test_unification('Test19', [f(g(X19), h(Y19, a), Z19) ?= f(g(b), h(c, a), k(t))], true),

    test_unification('Test20', [X20 ?= Y20, Y20 ?= Z20, Z20 ?= h(f(X20))], false),
    test_unification('Test21', [X21 ?= f(Y21,Z21), Y21 ?= g(a), Z21 ?= h(Y21)], true),
    test_unification('Test22', [f(a, X22) ?= Y22, Y22 ?= f(a, g(Z22))], true),
    test_unification('Test23', [X23 ?= f(Y23), Y23 ?= g(Z23, h(Z23)), Z23 ?= a], true),
    test_unification('Test24', [f(X24, Y24) ?= f(g(Y24), h(X24))], false),

    test_unification('Test25', [f(X25, g(Y25), Z25) ?= f(h(a), g(k(b)), m(T25)),
                                Y25 ?= k(b), Z25 ?= m(T25)], true),
    test_unification('Test26', [X26 ?= f(Y26,Z26), f(a,b) ?= X26, Z26 ?= b, Y26 ?= a], true),
    test_unification('Test27', [A27 ?= f(B27), B27 ?= g(C27), C27 ?= h(D27), D27 ?= A27], false),

    test_unification('Test28', [f(X28, g(Y28), h(Z28,Z28)) ?= f(g(a), g(k(W28)), h(W28, h(a))),
                                Z28 ?= h(a), Y28 ?= k(W28)], true).

    %test_unification('Test29', , false),
    %test_unification('Test30', , false),
    %test_unification('Test31', , false),
    %test_unification('Test32', , false),
    %test_unification('Test33', , false),
    %test_unification('Test34', , false),
    %test_unification('Test35', , false),
    %test_unification('Test36', , false),
    %test_unification('Test37', , false),
    %test_unification('Test38', , false),*/



