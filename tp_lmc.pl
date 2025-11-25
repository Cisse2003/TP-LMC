:- op(20,xfy,?=).

% Prédicats d'affichage fournis

% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- assert(echo_on).

% clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionné, echo(T) affiche le terme T
%          sinon, echo(T) réussit simplement en ne faisant rien.

echo(T) :- clause(echo_on, _), !, write(T) .
echo(_).

ligne :-
    clause(echo_on,_), !,
    nl.   % appel au nl standard
ligne.

% Question 1 : Implementation unifie(P)
    % Teste si la variable V apparaît dans le terme T
        occur_check(V, T) :- V == T,!.                    % cas trivial
        occur_check(V, T) :- var(V), nonvar(T), sub_term(Z, T), Z == V,!.

%% ----------------------------
% Détermine la règle de transformation R qui s'applique à l'équation E
        % Si S et T variables : rename si différente
            regle(S ?= T, rename) :- var(S), var(T), S \== T.

        % Si S et S variables  :  delete (x=x)
            regle(S ?= S, delete) :- var(S). % Pour x=x ou t=t
            regle(S ?= S, delete) :- atomic(S). % Pour x=x ou t=t


        % Si T constante et S variable : simplify
            regle(E, simplify) :- E = S ?= T, var(S),atomic(T), S \== T.

        % Si T composé et S variable, sans occurence : expand
            regle(S ?= T, expand) :- var(S), compound(T), \+ occur_check(S, T).

        % Si S non variables et T variable : orient
            %regle(S ?= T, orient) :- nonvar(S), var(T).
            regle(T ?= X, orient) :- var(X), \+ var(T).

        % Si même foncteur et même arité : decompose
            regle(S ?= T, decompose) :- compound(S), compound(T), functor(S, F, N), functor(T, F, N).

        % Si les foncteur sont différents : clash
            regle(S ?= T, clash) :- compound(S), compound(T), functor(S, F1, N1), functor(T, F2, N2), ( F1 \== F2 ; N1 \== N2 ).

        % Si variable = composé avec occurence : check
            regle(S ?= T, check) :- var(S),compound(T), occur_check(S, T).

    % Transforme le système d'équations P en le système d'équations Q par application de la règle de transformation R à l'équation E.

        % delete : on enlève l'équation
            reduit(delete, _ ?= _, P, P).        % select(X,Y,Z) supprime x dans y et mets le résultat dans z

        % rename : on peut remplacer la variable S par T partout
            reduit(rename, S ?= T, P, Q) :- S=T, subst(S,T,P,Q).
        % simplify : idem que rename
            reduit(simplify, S ?= T, P, Q) :-  S=T, subst(S,T,P,Q).

            reduit(expand, S ?= T, P, Q) :- S=T, subst(S, T, P, Q ).

        % orient : échange S et T
            reduit(orient, T ?= S, P,[S ?= T | P]).

        % decompose : remplace S?=T par leurs sous-termes
            reduit(decompose, S ?= T, P, Q) :-
                S =.. [_|ArgsS],
                T =.. [_|ArgsT],
                pairs_to_eqs(ArgsS, ArgsT, EqList),
                append(EqList, P, Q).
        % clash : on ne peut pas continuer
            reduit(clash, _, _, _) :-
                echo('Erreur : clash entre fonctions'), ligne,
                fail.
        % check : variable occurence détectée
        reduit(check, _, _, _) :-
            echo('Erreur : occur check échoué'), ligne,
            fail.
        % remplace profondément la variable X par T dans le terme InTerm -> OutTerm
            term_subst(X, T, InTerm, OutTerm) :-
                InTerm == X, !,                 % même identificateur => remplacer
                OutTerm = T.
            term_subst(_, _, InTerm, InTerm) :-
                atomic(InTerm), !.              % atomes / constantes inchangés
            term_subst(_, _, InTerm, InTerm) :-
                var(InTerm), !.                 % variable différente non remplacée
            term_subst(X, T, InTerm, OutTerm) :- % terme composé : on descend
                InTerm =.. [F|Args],
                maplist(term_subst(X, T), Args, Args2),
                OutTerm =.. [F|Args2].
        % remplace toutes les occurrences de X par T dans une liste de
            subst(_, _, [], []).
            subst(X, T, [H|Tail], [H2|Tail2]) :-
                H =.. [Op, A, B],
                term_subst(X, T, A, A2),
                term_subst(X, T, B, B2),
                H2 =.. [Op, A2, B2],
                subst(X, T, Tail, Tail2).

        %Transforme deux listes en équations
        pairs_to_eqs([], [], []).
        pairs_to_eqs([H1|T1], [H2|T2], [H1 ?= H2|EqT]) :-
            pairs_to_eqs(T1, T2, EqT).

        %unifie(P) :- set_echo,
        %                 unifie(P, []).

        %unifie([],Q) :-
        %    echo(Q), ligne,
        %    echo('Yes').

        %unifie(P,Q) :-
        %    echo('system: '), echo(P), ligne,
        %    P = [E|Rest],  % Choisir la première équation E dans P
        %    (regle(E, R) ->
        %    echo(R), echo(': '), echo(E), ligne,
        %    reduit(R, E, Rest, S),
        %    unifie(S,Q);
        %    echo('check: '), echo(E), ligne,fail).

% Question 2 : Implantation des stratégies de choix d’équation pour l’unification
    unifie(P) :- unifie(P, choix_premier).

    %unifie(P, Strategie) :- set_echo, unifie(P, [], Strategie).
    unifie(P, Strategie) :- unifie(P, [], Strategie).

    unifie([],_, _) :-true.

    unifie(P, Q, Strategie) :-
        choix(P, Q_rest, E, R, Strategie),
        echo('system : '), echo(P), ligne,
        echo(R), echo(': '), echo(E), ligne,
        reduit(R, E, Q_rest, S),
        unifie(S, Q, Strategie).

    % Choix systématique de la première équation
    choix([E|Q_rest], Q_rest, E, R, choix_premier) :-
        regle(E, R)->true;echo('check: '), echo(E), ligne,fail.                                        % R = règle applicable à E

    %implementation des 2 choix pondérée
    choix(P, Q_rest, E, R, choix_pondere_1) :- choix_pondere(P, Q_rest, E, R, 1).
    choix(P, Q_rest, E, R, choix_pondere_2) :- choix_pondere(P, Q_rest, E, R, 2).
    choix(P, Q_rest, E, R, choix_random) :- choix_random(P, Q_rest, E, R).

    % Implémentation d'une stratégie pondérée
        % Plus le nombre est grand, plus la règle est prioritaire
            poids(1,clash, 6).
            poids(1,check, 6).
            poids(1,rename, 5).
            poids(1,simplify, 5).
            poids(1,orient, 4).
            poids(1,decompose, 3).
            poids(1,expand, 2).
            poids(1,delete, 1).

        % Deuxieme stratégie de choix pondérer Plus le nombre est grand, plus la règle est prioritaire
            poids(2,simplify, 5).
            poids(2,rename, 5).
            poids(2,decompose, 3).
            poids(2,orient, 4).
            poids(2,expand, 2).
            poids(2,clash, 6).
            poids(2,check, 6).
            poids(2,delete, 1).

        % Prédicat de choix pondéré
            choix_pondere(P, Q_rest, E, R,I) :-
                % Pour chaque equation, on calcul la règle et son poids, la fonction genere toutes les combinaisons
                findall([Eq, Regle, Pds], (member(Eq, P),
                regle(Eq, Regle)->true;echo('check: '), echo(Eq), ligne,fail,
                poids(I,Regle, Pds)), L),L \= [],
                % On prend l'equation de poids maximal
                poids_max(L, [E, R, _]),
                select(E, P, Q_rest).    % Q_rest quotient le reste du systeme sans l'equation choisie

            % poids_max(Liste, EleMax), renvoie dans EleMax l'entree de L avec le plus grand poids
            poids_max([H], H) :- !.  % cas de base, un seul element alors c'est le max
            poids_max([[Eq,R,Pd]|Reste], Max) :- poids_max(Reste,nouveauMax),nouveauMax=[_,_,nouveauPd],( Pd >= nouveauPd -> Max = [Eq,R,Pd] ; Max = nouveauMax ).


    %Autres stratégies possibles( on a implementer une stratégie random qui choisis les équation aleatoirement)


        %implementation d'un choix ramdom
        %la fonction random_member nous permet de choisir une equation aleatoirement dans P
        choix_random(P, Q_rest, E, R) :- random_member(E, P),
        regle(E, R)
        ,select(E, P, Q_rest).

% Question 3 : Implémentation de unif et de trace_unif

    % unif( P , S ) permet de faire l'unification sans afficher les traces des différents règles utiliser
    unif(P, S) :-
        clr_echo,               % désactiver l'affichage
        unifie(P, S).           % appeler ta version existante

    % trace_unif(P, S) permet de faire l'unification sans afficher les différents règles utiliser
    trace_unif(P, S) :-
        set_echo,               % activer l'affichage
        unifie(P, S).           % appeler ta version existante

