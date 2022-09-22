; Language spec
;
; Dynamically and weakly typed, interpreted
; Impure functions marked with ! (convention)
; Boolean functions marked with ? (convention)
; Loops implemented with recursion
;
; Value types:
;   Number      -3.14
;   Symbol      PI
;   Boolean     true
;   String      "foobar"
;   Vector      [1 2 3]
;   Hash map    { a 1 b 2 }
;   Function    (\[x y] (+ x y))
;
; Syntax
;   Function calls
;       A sequence of values surrounded by parens is considered a function call
;       In a function call, the first element must evaluate to a function value.
;       The function will be invoked with the rest of the elements as arguments to the function.
;   Function definition
;       Calling the builtin variadic function '\' constructs a new function. The first argument
;       must be a vector of symbols (parameter list). The rest of the arguments form the function
;       body. The evaluated value of the last argument is returned as the return value of the
;       function.
;
; Some builtin functions
;   let         (let sum2 (\[x y] (+ x y)))    Evaluates second argument and stores value in environment
;   \           (\[x y] + x y)
;   print!      (print! PI)
;   if          (if (lt? 10 5)
;                   (print "10 is less than 5")
;                   (print "10 is not less than 5"))
;   match       (match (sum2 1 1)
;                   2 (do-thing)
;                   3 (do-other-thing)
;                   (do-else))
;   head        (head vec)
;   tail        (tail vec)
;   prep        (prep x xs)
;
; Functions are curried
; E.g.
;   (let f1 (\[x y] (+ x y)))
;   (let f2 (\[x] (\[y] (+ x y))))
;   ; f1 == f2

(let PI 3.14159)
(let circle-area (\[r]
    (let rr (mul2 r r))
    (mul2 PI rr)))

(let fibo (\[n]
    (let fibo-1 (\[] (fibo (sub2 n 1))))
    (let fibo-2 (\[] (fibo (sub2 n 2))))
    (match n
        0  0
        1  1
        (sum2 (fibo-1) (fibo-2)))))

(print! (fibo 5))
; 5

(let map (\[f lst]
    (match lst
        [] []
        (prep (f head lst) (map f (tail lst))))))

(print!
    (map (sum2 1) [1 2 3]))
; 2 3 4

(let foldr (\[f accumulator lst]
    (match lst
        []  accumulator
        (f (foldr f accumulator (tail lst)) (head lst)))))

(print!
    (foldr sum2 0 [1 2 3]))
; 6
