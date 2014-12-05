flight(istanbul, newyork).
train(newyork, newhaven).
train(newhaven, meriden).
bus(meriden, middletown).

reachable(X,Y) :- flight(X,Y).
reachable(X,Y) :- flight(Y,X).
reachable(X,Y) :- train(X,Y).
reachable(X,Y) :- train(Y,X).
reachable(X,Y) :- bus(X,Y).
reachable(X,Y) :- bus(Y,X).
reachable(X,Y) :- reachable(X,Z), reachable(Z,Y).
