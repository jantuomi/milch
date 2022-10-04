; LIB

(let! just (\[a]
    ["maybe" "just" a]))
(let! nothing (\[]
    ["maybe" "nothing"]))

(let! unsafe-at (\[n seq]
    (match seq
        []
            (fatal! "unsafe-at out of bounds")
        otherwise
            (match n
                0
                    (head seq)
                otherwise
                    (unsafe-at (- n 1) (tail seq))))))

(let! unpack-just (unsafe-at 2))
(let! kind (unsafe-at 1))

(let! map (\[f m]
    (match (kind m)
        "just" (just (f (unpack-just m)))
        "nothing" (nothing))))

(let! and-then (\[f m]
    (match (kind m)
        "just" (f (unpack-just m))
        "nothing" (nothing))))

; TESTING

;; (let! print-line! (\[s]
;;     (print! (concat s "\n"))))

;; (let! a (just 10))
;; (let! b (nothing))

;; (map (+ 5) a)
;; (map (+ 5) b)

;; (and-then (\[n] (just (+ n 5))) a)
;; (and-then (\[n] (just (+ n 5))) b)

;; (let! m (just 10))
;; (match m
;;     (just _)
;;         (print-line! (fmt "found just {0}!" [(unpack-just m)]))
;;     (nothing)
;;         (print-line! "found nothing!"))

(let! lazy exports [
    just
    nothing
    unpack-just
    kind
    map
    and-then
])
