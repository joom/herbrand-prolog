herbrand-prolog [![Build Status](https://secure.travis-ci.org/joom/herbrand-prolog.svg)](http://travis-ci.org/joom/herbrand-prolog)
=====

A pseudo-Prolog that tries to answer queries by building the least Herbrand model.

Written as a final project for COMP360 Computational Logic and Logic Programming, Fall 2014, Prof. Lipton, Wesleyan University.

[Here's a blog post about the development process of the project.](http://cattheory.com/posts/2014-12-05-herbrand.html)

## Limitations

* Currently, you cannot use functions in your program, such as `s(o)`. The parser doesn't handle them yet, neither does the least Herbrand model builder.

* You cannot use `;` as "or". Try adding separate Horn clauses to the program for this.

* You cannot use `\+`, `!.`.

* You cannot use natural numbers or lists. You can only use constants and relations.

## Usage

In order to use or compile the program you need to have [Haskell](http://www.haskell.org/) installed.

After you cloning the repository, go the repository folder and do

```bash
cabal build
```

Now you compiled the program. There is an example Prolog file in the "usage" folder. You can run it like this:

```bash
./dist/build/repl/repl usage/travel.prolog
```

Then you can ask questions like `reachable(istanbul, newhaven).`

Here's an example REPL session for the Prolog program we have in usage/travel.prolog:

```
herbrand-prolog REPL
?- reachable(istanbul, newhaven).
yes
?- reachable(meriden, X).
Possible answers:
[reachable/2(meriden, meriden),reachable/2(meriden, middletown),reachable/2(meriden, newhaven),reachable/2(meriden, istanbul),reachable/2(meriden, newyork)]
?- reachable(istanbul, london).
no
```

Note that the system build a language from your program, that has all the constants used in your program. This means that if you define a predicate like `always_say_yes(X,Y).`, and try `always_say_yes(hello,world).`, the system will say no, because `hello` and `world` are not in the language (unless they are used somewhere else in the program) and therefore the system doesn't include them into the least Herbrand model, since it tries to build the entire list of ground formulae.

## License

[MIT License](http://joom.mit-license.org)
