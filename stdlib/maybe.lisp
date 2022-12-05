; LIB

(let! Maybe/just (\[a]
    ["Maybe" "just" a]))
(let! Maybe/nothing (\[]
    ["Maybe" "nothing"]))

(let! Maybe/#at (\[n seq]
    (match seq
        []
            (fatal! "Maybe/#at out of bounds")
        otherwise
            (match n
                0
                    (head seq)
                otherwise
                    (Maybe/#at (- n 1) (tail seq))))))

(let! Maybe/unpack (Maybe/#at 2))
(let! Maybe/kind (Maybe/#at 1))

(let! Maybe/map (\[f m]
    (match (Maybe/kind m)
        "just" (Maybe/just (f (Maybe/unpack m)))
        "nothing" (Maybe/nothing))))

(let! Maybe/and-then (\[f m]
    (match (Maybe/kind m)
        "just" (f (Maybe/unpack m))
        "nothing" (Maybe/nothing))))

; TESTING

;; (let! print-line! (\[s]
;;     (print! (concat s "\n"))))

;; (let! a (Maybe/just 10))
;; (let! b (Maybe/nothing))

;; (Maybe/map (+ 5) a)
;; (Maybe/map (+ 5) b)

;; (Maybe/and-then (\[n] (Maybe/just (+ n 5))) a)
;; (Maybe/and-then (\[n] (Maybe/just (+ n 5))) b)

;; (let! m (Maybe/just 10))
;; (match m
;;     (Maybe/just _)
;;         (print-line! (fmt "found just {0}!" [(Maybe/unpack m)]))
;;     (Maybe/nothing)
;;         (print-line! "found nothing!"))
