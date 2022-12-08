# MILCH: [M]y functional language [I]nspired by [L]isp, [C]lojure, and [H]askell

## Summary

`MILCH` (or `Milch`, or `milch`) is a interpreted, dynamically typed functional programming language that has some Lisp-inspired syntax and Haskell-inspired semantics. Milch is German for 🥛.

I'm developing this language by myself as a hobby, and the spec is going to change all the time. Some features documented here might not yet be implemented.

Check out the language spec  
[spec.md](spec.md)

Check out the TODO  
[todo.md](todo.md)

## Building

Install a Haskell toolchain with Stack and GHC. To build and install the `milch` binary in your `PATH`, run:

    stack install

To see the help text, run:

    milch -h

## Tests

To run the test suite, run:

    stack test

## Author

Jan Tuomi <<jans.tuomi@gmail.com>>
