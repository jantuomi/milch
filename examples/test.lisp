;; (let map (\[f lst]
;;     (match lst
;;         [] []
;;         (prepend (f head lst) (map f (tail lst))))))

;; (map (+ 1) [1 2 3])

(let mapinc (\[vec]
    (match vec
        [] []
        (prepend
            (+ 1 (head vec))
            (mapinc (tail vec))))))
(env)
(mapinc [1 2 3])
