; LIB

(union! Maybe
    (just value)
    (nothing))

(let! Maybe/map (\[f m]
    (match (kind m)
        "Maybe/Just" (Maybe/just (f (Maybe/Just/get-value m)))
        "Maybe/Nothing" (Maybe/nothing))))

(let! Maybe/and-then (\[f m]
    (match (kind m)
        "Maybe/Just" (f (Maybe/Just/get-value m))
        "Maybe/Nothing" (Maybe/nothing))))

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
;;         (print-line! (fmt "found just {0}!" [(Maybe/Just/get-value m)]))
;;     (Maybe/nothing)
;;         (print-line! "found nothing!"))

;; (let! m (Maybe/just 10))
;; (pipe m [
;;     (Maybe/map (+ 5))
;;     (Maybe/and-then (\[val] (Maybe/just (- val 3))))
;; ])
