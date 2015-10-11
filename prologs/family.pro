father(chirag,heman).
mother(trupti,heman).
male(heman).

father(chirag,tanvi).
mother(trupti,tanvi).
female(tanvi).

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

mother(shobana, chirag).
father(sharat, chirag).

mother(shobana, prachi).
father(sharat, prachi).

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

mother(prachi, ketaki).
father(kiran, ketaki).
female(ketaki).

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

father(chandrakant, trupti).
mother(heera, trupti).

father(chandrakant, deepak).
mother(heera, deepak).

father(deepak, dhara).
father(deepak, neel).
father(deepak, keya).

mother(heena, dhara).
mother(heena, neel).
mother(heena, keya).

female(dhara).
female(keya).
male(neel).
















