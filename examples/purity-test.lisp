(let! f (\[x] x))
(f 10)

(let! g! (\![x] (print! x)))
(g! "foo")

(let! h (\[x] (g! x)))
(h "bar")

(let! main! (\![]
    (print! (fmt "{0}\n" ["main"]))
    ))

(main!)

(let! pure-main (\[]
    (print! (fmt "{0}\n" ["pure-main"]))
    ))

(pure-main)
