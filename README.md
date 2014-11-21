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

Since our program is going to proceed by trying to find the query in the created Herbrand base, we should make sure that it doesn't try to construct the entire Herbrand base first. This would cause the program to fail whenever the program has a predicate like the example above. (`add/3`) Haskell's lazy evaluation is a perfect environment to avoid this. However, we cannot use the built-in Data.Set type (which is not lazy) in Haskell, we need another data type to represent an infinite set. We also need this infinite set structure to handle set unions with lazy evaluation, and we need to check membership without getting stuck on the first element of a set union.

For example, for the `add/3` example above, if the query happens to be `add(s(o), o, s(o))`, the program should not get stuck in `T_P({})`, which is an infinite set. The answer is in `T_P(T_P({}))`, which is the second set in the union `M_P`. Our infinite set data type should be able to handle this kind of issue.

However, what makes this project a pseudo-Prolog is that, it can never say `no` for a predicate like `add`. It will check membership in `M_P` forever and it will never find one for an incorrect query, such as `add(s(o), o, o)`. A real Prolog implementation get the result `no` with SLD resolution, which is not what this project does. (again, *pseudo*-Prolog)

## License

[MIT License](http://joom.mit-license.org)
