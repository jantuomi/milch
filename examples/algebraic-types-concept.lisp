(let just (\[a]
    ["maybe" "just" a]))
(let nothing (\[]
    ["maybe" "nothing"]))

(let at (\[n seq]
    (match seq
        [] (nothing)
        (match n
            0 (head seq)
            (at (- n 1) (tail seq))))))

(let from-just (at 2))
(let kind (at 1))

(let m (just 10))
(match (kind m)
    "just" (from-just m)
    "nothing" "nothing")
