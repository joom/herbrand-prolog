mother(marge, lisa).
father(homer, lisa).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).
