(let map (\[f lst]
    (match lst
        [] []
        (prepend (f (head lst)) (map f (tail lst))))))

(map (+ 1) [1 2 3])

(let foldr (\[f accumulator lst]
    (match lst
        []  accumulator
        (f (foldr f accumulator (tail lst)) (head lst)))))

(foldr + 0 [1 2 3])

(let filter (\[pred lst]
    (match lst
        [] []
        (match (pred (head lst))
            true (prepend (head lst) (filter pred (tail lst)))
            false (filter pred (tail lst))))))

(let pred (\[n]
    (match n
        2 true
        4 true
        false)))

(filter pred [0 1 2 3 4 5])

;; (let fibo (\[n]
;;     (match n
;;         0  0
;;         1  1
;;         (+
;;             (fibo (- n 1))
;;             (fibo (- n 2))))))

;; (fibo 10)

(let fibo (\[n]
    (let lazy fibo-1 (fibo (- n 1)))
    (let lazy fibo-2 (fibo (- n 2)))
    (match n
        0  0
        1  1
        (+ fibo-1 fibo-2))))

(fibo 10)

(let mod (\[n k]
    (- n (* k (/ n k)))))
