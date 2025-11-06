:- op(20,xfy,?=).

% Prédicats d'affichage fournis

% set_echo: ce prédicat active l'affichage par le prédicat echo
set_echo :- assert(echo_on).

% clr_echo: ce prédicat inhibe l'affichage par le prédicat echo
clr_echo :- retractall(echo_on).

% echo(T): si le flag echo_on est positionné, echo(T) affiche le terme T
%          sinon, echo(T) réussit simplement en ne faisant rien.

echo(T) :- clause(echo_on, _), !, write(T) nl.
echo(_).


% Question 1 : Implementation unifie(P)

    % Détermine la règle de transformation R qui s'applique à l'équation E

        % Si S et T variables : rename si différente, sinon delete (x=x)
            regle(S ?= T, rename) :- var(S), var(T), S \== T.

            regle(S ?= T, delete) :- S == T. % Pour x=x ou t=t

        % Si T constante et S variable : simplify
            regle(S ?= T, simplify) :- var(S), atomic(T).

        % Si T composé et S variable, sans occurence : expand
            regle(S ?= T, expand) :- var(S), compound(T), \+ occur_check(S, T).

        % Si S non variables et T variable : orient
            regle(S ?= T, orient) :- nonvar(S), var(T).

        % Si même foncteur et même arité : decompose
            regle(S ?= T, decompose) :- compound(S), compound(T), functor(S, F, N), functor(T, F, N).

        % Si les foncteur sont différents : clash
            regle(S ?= T, clash) :- compound(S), compound(T), functor(S, F1, N1), functor(T, F2, N2), ( F1 \== F2 ; N1 \== N2 ).

        % Si variable = composé avec occurence : check
            regle(S ?= T, check) :- var(S), compound(T), occur_check(S, T).




    % Teste si la variable V apparaît dans le terme T

        occur_check(V, T) :- var(V), nonvar(T), sub_term(Z, T), Z == V.        % sub_term, afin de vérifier si Z est un sous_terme de T
                                                                                   % et Z == V vérifie si Z à le même identifiant interne que V afin de comparer sans instancier

    % Transforme le système d'équations P en le système d'équations Q par application de la règle de transformation R à l'équation E.

        % delete : on enlève l'équation
            appliquer_regle(delete, E, P, Q) :- select(E, P, Q).        % select(X,Y,Z) supprime x dans y et mets le résultat dans z

        % rename : on peut remplacer la variable S par T partout
            appliquer_regle(rename, S ?= T, P, Q) :- subst_var(S,T,P,Q).

        % simplify : idem que rename
            appliquer_regle(simplify, S ?= T, P, Q) :- subst_var(S,T,P,Q).

        % orient : échange S et T
            appliquer_regle(orient, S ?= T, P, Q) :- select(S ?= T, P, Temp), Q = [T ?= S | Temp].

        % decompose : remplace S?=T par leurs sous-termes
            appliquer_regle(decompose, S ?= T, P, Q) :-
                S =.. [_|ArgsS],
                T =.. [_|ArgsT],
                maplist(pair_eq, ArgsS, ArgsT, EQs)., % pair_eq recupere les elements de ArgsS et de ArgsH pour crée les equations
                select(S ?= T, P, Temp),
                append(EQs, Temp, Q).
            pair_eq(A,B,A ?= B).
        % clash : on ne peut pas continuer
            appliquer_regle(clash, _, _, _) :-
                write('Erreur : clash entre fonctions'), nl,
                fail.

        % check : variable occurence détectée
        appliquer_regle(check, _, _, _) :-
            write('Erreur : occur check échoué'), nl,
            fail.

        % remplace toutes les occurrences de Var par Val dans une liste de termes
            subst_var(_, _, [], []).
            %maplist applique subst_in_term sur tous les argument de H et mets le resultats dans H1
            subst_var(Var, Val, [H|T], [H1|T1]) :- H1 =.. [F|ArgsH], maplist(subst_in_term(Var, Val), ArgsH, ArgsH1),H1 =.. [F|ArgsH1], subst_var(Var, Val, T, T1).

            subst_in_term(Var, Val, Term, Term1) :- var(Term),Term == Var -> Term1 = Val ;
                    compound(Term) ->
                    Term =.. [F|Args],
                    maplist(subst_in_term(Var, Val), Args, Args1),
                    Term1 =.. [F|Args1] ;
                    Term1 = Term.


        reduit(R, E, P, Q) :- regle(E, R), appliquer_regle(R, E, P, Q).




