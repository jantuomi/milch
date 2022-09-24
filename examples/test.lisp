;; (let map (\[f lst]
;;     (match lst
;;         [] []
;;         (prepend (f (head lst)) (map f (tail lst))))))

;; (map (+ 1) [1 2 3])

;; (let mapinc (\[vec]
;;     (match vec
;;         [] []
;;         (prepend
;;             (+ 1 (head vec))
;;             (mapinc (tail vec))))))
;; (env)
;; (mapinc [1 2 3])

;; (let fibo (\[n]
;;     ;; (let fibo-1 (\[] (fibo (sub2 n 1))))
;;     ;; (let fibo-2 (\[] (fibo (sub2 n 2))))
;;     (match n
;;         0  0
;;         1  1
;;         (+
;;             (fibo (- n 1))
;;             (fibo (- n 2))))))

;; (fibo 10)

(let foldr (\[f accumulator lst]
    (match lst
        []  accumulator
        (f (foldr f accumulator (tail lst)) (head lst)))))

(foldr + 0 [1 2 3])
