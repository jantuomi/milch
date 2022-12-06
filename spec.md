# Language spec

- Dynamically and weakly typed, interpreted
- Impure functions marked with ! (convention)
- Boolean functions marked with ? (convention)
- Loops implemented with recursion

## Value types

  Number      -3.14
  Symbol      PI
  Boolean     true
  String      "foobar"
  Vector      [1 2 3]
  Hash map    { a 1 b 2 }
  Function    (\[x y] (+ x y))

## Syntax

### Function calls

A sequence of values surrounded by parens is considered a function call In a function call, the first element must evaluate to a function value. The function will be invoked with the rest of the elements as arguments to the function.

### Function definition
Calling the builtin variadic function '\' constructs a new function. The first argument must be a vector of symbols (parameter list). The rest of the arguments form the function body. The evaluated value of the last argument is returned as the return value of the function.

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

    (\[x y] + x y)

`print!`  
Prints a value and a newline.

    (print! PI)

`match`  
Matches first argument (value) to even positioned arguments (matchers). On match (at position i), evaluates the odd positioned argument (branch) right after it (at position i + 1). If given an even number of arguments, the last argument will act as the default branch.

    (match (sum2 1 1)
        2 (do-thing)
        3 (do-other-thing)
        (do-else))

`head`  
Gets the first element of a vector.

    (head vec)

`tail`  
Drops the first element of a vector.

    (tail vec)

`prepend`  
Pushes the first argument to the front of the second argument (vector).

    (prepend x xs)
