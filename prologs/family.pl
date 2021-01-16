% Atoms: parent, married, male, female.
wedded(X, Y) :-
    married(X, Y).
wedded(X, Y) :-
    married(Y, X).

% Helps with gujurati.
mother(X, Y) :-
    parent(X, Y), female(X).
father(X, Y) :-
    parent(X, Y), male(X).

siblingOrCousin(X, Y) :-
    mother(Z, X),
    mother(Z, Y),
    father(W, X),
    father(W, Y),
    dif(X, Y).
siblingOrCousin(X, Y) :-
    parent(Z, X),
    parent(W, Y),
    siblingOrCousin(Z, W).

% Gujurati familial terms

gujurati_term(X, X, []).

%% Simple cases: the term is just a relation
% Parents
gujurati_term(X, Y, [mumi | R]) :-
    mother(X, Z),
    gujurati_term(Z, Y, R).
gujurati_term(X, Y, [puppa | R]) :-
    father(X, Z),
    gujurati_term(Z, Y, R).

% Siblings and cousins.
gujurati_term(X, Y, [bhai | R]) :-
    siblingOrCousin(X, Z), male(X),
    gujurati_term(Z, Y, R).
gujurati_term(X, Y, [bhen | R]) :-
    siblingOrCousin(X, Z), female(X),
    gujurati_term(Z, Y, R).

%% Compound cases: we can break the term into other terms.
% Grandparents
gujurati_term(X, Y, [dada | R]) :-
    gujurati_term(X, Y, [puppa, puppa | R]).

gujurati_term(X, Y, [dadi | R]) :-
    gujurati_term(X, Y, [puppa, mumi | R]).

gujurati_term(X, Y, [nani | R]) :-
    gujurati_term(X, Y, [mumi, mumi | R]).

gujurati_term(X, Y, [nana | R]) :-
    gujurati_term(X, Y, [mumi, puppa | R]).

% Mother's siblings
gujurati_term(X, Y, [mama | R]):-
    gujurati_term(X, Y, [mumi, bhai | R]).
gujurati_term(X, Y, [masi | R]):-
    gujurati_term(X, Y, [mumi, bhen | R]).

% Father's siblings
gujurati_term(X, Y, [phoi | R]) :-
    gujurati_term(X, Y, [puppa, bhen | R]).
gujurati_term(X, Y, [kaka | R]) :-
    gujurati_term(X, Y, [puppa, bhai | R]).

%% Super-compounds: marriage
gujurati_term(X, Y, [mami | R]):-
    wedded(X, Z),
    gujurati_term(Z, Y, [mumi, bhai | R]).
gujurati_term(X, Y, [masa | R]):-
    wedded(X, Z),
    gujurati_term(Z, Y, [mumi, bhen | R]).
gujurati_term(X, Y, [phua | R]) :-
    wedded(X, Z),
    gujurati_term(Z, Y, [puppa, bhen | R]).
gujurati_term(X, Y, [kaki | R]) :-
    wedded(X, Z),
    gujurati_term(Z, Y, [puppa, bhai | R]).


