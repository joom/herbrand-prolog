herbrand-prolog [![Build Status](https://secure.travis-ci.org/joom/herbrand-prolog.svg)](http://travis-ci.org/joom/herbrand-prolog)
=====

A pseudo-Prolog that tries to answer queries by building the least Herbrand model.

Written as a final project for COMP360 Computational Logic and Logic Programming, Fall 2014, Prof. Lipton, Wesleyan University. In progress.

## Limitations

* Currently, you cannot use functions in your program, such as `s(o)`. The parser doesn't handle them yet, neither does the least Herbrand model builder.

* You cannot use `;` as "or". Try adding separate Horn clauses to the program for this.

* You cannot use `\+`, `!.`.

* You cannot use natural numbers.

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

## License

[MIT License](http://joom.mit-license.org)
