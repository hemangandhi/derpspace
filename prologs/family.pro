father(dad,heman).
mother(mom,heman).
male(heman).

father(dad,sister).
mother(mom,sister).
female(sister).

male(X):- father(X,Y).
female(X):- mother(X,Y).

parent(X,Y):- father(X,Y).
parent(X,Y):- mother(X,Y).

sibling(X,Y) :-
	parent(Z,X),
	parent(Z,Y),
	X \== Y.

sister(X,Y):-
	sibling(X,Y),
	female(X).

brother(X,Y):-
	sibling(X,Y),
	male(X).

married(X,Y):-
	parent(X,Z),
	parent(Y,Z).

wife(X,Y):-
	married(X,Y),
	female(X).

husband(X,Y):-
	married(X,Y),
	male(X).

mother(grandma, dad).
father(grandpa, dad).

mother(grandma, aunt1).
father(grandpa, aunt1).

grandparent(X,Y):-
	parent(Z,Y),
	parent(X,Z).

grandmother(X,Y):-
	grandparent(X,Y),
	female(X).

grandfather(X,Y):-
	grandparent(X,Y),
	male(X).

aunt(X,Y):-
	parent(Z,Y),
	sister(X,Z).

uncle(X,Y):-
	parent(Z,Y),
	brother(X,Z).

aunt(X,Y):-
	uncle(Z,Y),
	wife(X,Z).

uncle(X,Y):-
	aunt(Z,Y),
	husband(X,Z).

mother(aunt1, cousin1).
father(uncle1, cousin1).
female(cousin1).

cousin(X,Y):-
	aunt(Z,Y),
	mother(Z,X).

cousinSister(X,Y):-
	cousin(X,Y),
	female(X).

cousinBrother(X,Y):-
	cousin(X,Y),
	male(X).

niece(X,Y) :-
	female(X),
	uncle(Y,X).

niece(X,Y):-
	female(X),
	aunt(Y,X).

nephew(X,Y):-
	uncle(Y,X),
	male(X).

nephew(X,Y):-
	aunt(Y,X),
	male(X).

son(X,Y) :- parent(Y,X), male(X).
daughter(X,Y) :- parent(Y,X), female(X).

father(mgrandpa, mom).
mother(mgrandma, mom).

father(mgrandpa, uncle2).
mother(mgrandma, uncle2).

father(uncle2, cousin2).
father(uncle2, cousin3).
father(uncle2, cousin4).

mother(aunt2, cousin2).
mother(aunt2, cousin3).
mother(aunt2, cousin4).

female(cousin2).
female(cousin4).
male(cousin3).
















