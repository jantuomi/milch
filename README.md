# MILCH: [M]y functional language [I]nspired by [L]isp, [C]lojure, and [H]askell

## Summary

`MILCH` (or `Milch`, or `milch`) is a interpreted, dynamically typed functional programming language that has some Lisp-inspired syntax and Haskell-inspired semantics. Milch is german for 🥛.

I'm developing this language by myself as a hobby, and the spec is going to change all the time. Some features documented here might not yet be implemented.

Check out the language spec  
[spec.md](spec.md)

Check out the TODO  
[todo.md](todo.md)

## Building

Install a Haskell toolchain with Stack and GHC. Run

    stack build
    stack install

to build and install the `milch` binary in your `PATH`.

Then, run

    milch -h

to see the help text.

## Author

Jan Tuomi <<jans.tuomi@gmail.com>>
