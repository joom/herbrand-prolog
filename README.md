# herbrand-prolog

A pseudo-Prolog that tries to answer queries by building the least Herbrand model.

Written as a final project for COMP360 Computational Logic and Logic Programming, Fall 2014, Prof. Lipton, Wesleyan University.

## Implementation Problems

Note that trying to find answers for queries by building the least Herbrand model is problematic in certain ways. First of all, for many programs, the least Herbrand model is infinite. For example,

```prolog
add(o,X,X).
add(s(X), Y, s(Z)) :- add(X,Y,Z).
```

will have an infinite least Herbrand model, which is like this:

```
T_P({}) = {add(o,o,o), add(o,s(o), s(o)), add(o, s(s(o)), s(s(o))), ...}
T_P(T_P({})) = {add(s(o), o, s(o)), add(s(o), s(o), s(s(o))), add(s(o), s(s(o)), s(s(s(o)))), ...}

M_P = T_P({}) ∪ T_P(T_P({})) ∪ ...
```

Note that it's not only that the result `T_P` on any set is infinite, but also `M_P` is constructed by the union of infinite number of sets. In this case, we need to make sure that our implementation doesn't get stuck in an infinite list.



## License

[MIT License](http://joom.mit-license.org)
