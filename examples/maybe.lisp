; LIB

(let just (\[a]
    ["maybe" "just" a]))
(let nothing (\[]
    ["maybe" "nothing"]))

(let unsafe-at (\[n seq]
    (match seq
        []
            (error "unsafe-at out of bounds")
        (match n
            0
                (head seq)
            (unsafe-at (- n 1) (tail seq))))))

(let unpack-just (unsafe-at 2))
(let kind (unsafe-at 1))

(let map (\[f m]
    (match (kind m)
        "just" (just (f (unpack-just m)))
        "nothing" (nothing))))

(let and-then (\[f m]
    (match (kind m)
        "just" (f (unpack-just m))
        "nothing" (nothing))))

; TESTING

(let print-line! (\[s]
    (print! (concat s "\n"))))

(let m (just 10))
(match (kind m)
    "just"
        (print-line! (fmt "found just {0}!" [(unpack-just m)]))
    "nothing"
        (print-line! "found nothing!"))

;; (export [
;;     just
;;     nothing
;;     unpack-just
;;     kind
;;     map
;;     and-then
;; ])