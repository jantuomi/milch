# Language spec

- Dynamically and weakly typed, interpreted
- Impure functions marked with ! (convention), purity is tracked by the interpreter
- Boolean functions marked with ? (convention)
- Loops implemented with recursion

## Values and types

    Integer     3
    Float       -3.14
    Symbol      PI
    Tag         :tag
    Boolean     true
    Char        'c'
    String      "foobar"
    Vector      [1 2 3]
    Hash map    { a 1 b 2 }
    Function    (\[x y] (+ x y))

Functions generally only accept monomorphic values, but some builtins are polymorphic, either expecting any arbitrary type (as is the case in e.g. `match` and `fmt`), or only a range of types (type classes). User defined functions can be made "polymorphic" by matching on the return value of `(type arg)`, where `arg` is the argument of the user defined function.

There are 2 language defined "type classes": `Num` and `Seq`. Integers and floats are instances of `Num`, while Strings and Vectors are instances of `Seq`. Type classes are not a thing per se in the interpreter, but they are a useful concept to "formalize" some builtin functions and their inputs. `Num` values can be e.g. added together with `+`. `Seq` values can be used to call functions like `head` and `tail` and `len`.

Collections (vectors, hash maps) are heterogenous, which means that a single collection can contains values of multiple types.

## Syntax

### Function calls

A sequence of values surrounded by parens is considered a function call In a function call, the first element must evaluate to a function value. The function will be invoked with the rest of the elements as arguments to the function. A function call with no arguments evaluates to the function itself.

### Function definition

Calling the builtin variadic function `\` constructs a new function. The first argument must be a vector of symbols (parameter list). The rest of the arguments form the function body. The evaluated value of the last argument is returned as the return value of the function.

Memoized pure functions can be created with a special `let memo`-binding:

    (let memo f (\[x] x))

Impure functions can be created with the builtin `\!`.

### Currying

Functions are automatically curried  
E.g.

    (let f1 (\[x y] (+ x y)))
    (let f2 (\[x] (\[y] (+ x y))))
    ; f1 equivalent to f2

## Some builtin functions

`let`  
Evaluates second argument and stores the resulting value in the environment.

    (let sum2 (\[x y] (+ x y)))

`\`  
Defines a function.

    (\[x y] (+ x y))

`print!`  
Prints a value without a newline.

    (print! PI)

`match`  
Matches first argument (value) to even positioned arguments (matchers). On match (at position i), evaluates the odd positioned argument (branch) right after it (at position i + 1).

    (match (sum2 1 1)
        2 (do-thing)
        3 (do-other-thing)
        otherwise (do-else))

`head`  
Gets the first element of a vector.

    (head vec)

`tail`  
Drops the first element of a vector.

    (tail vec)

`cons`  
Pushes the first argument to the front of the second argument (vector).

    (cons x xs)
