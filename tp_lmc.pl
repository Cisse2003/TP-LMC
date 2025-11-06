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

        % Si T constante et S variable : simplify
            regle(S ?= T, simplify) :- var(S), atomic(T).

        % Si T composé et S variable, sans occurence : expand
            regle(S ?= T, expand) :- var(S), compound(T), \+ occur_check(S, T).

        % Si S non variables et T variable : orient
            regle(S ?= T, orient) :- nonvar(S), var(T).

        % Si même foncteur et même arité : decompose
            regle(S ?= T, decompose) :- compound(S), compound(T), functor(S, F, N), functor(T, F, N).

        % Si les foncteurs sont différents : clash
            regle(S ?= T, clash) :- compound(S), compound(T), functor(S, F1, N1), functor(T, F2, N2), ( F1 \== F2 ; N1 \== N2 ).

        % Si variable = composé avec occurence : check
            regle(S ?= T, check) :- var(S), compound(T), occur_check(S, T).

        regle(S ?= T, delete) :- S == T. % Pour x=x ou t=t


        % Teste si la variable V apparaît dans le terme T

            occur_check(V, T) :- var(V), nonvar(T), sub_term(Z, T), Z == V.        % sub_term, afin de vérifier si Z est un sous_terme de T
                                                                                   % et Z == V vérifie si Z à le même identifiant interne que V afin de comparer sans instancier




