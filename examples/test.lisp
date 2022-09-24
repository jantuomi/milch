;; map :: (a -> b) -> [a] -> [b]
(let map (\[f lst]
    (match lst
        [] []
        (prepend (f (head lst)) (map f (tail lst))))))

(map (+ 1) [1 2 3])

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(let foldr (\[f accumulator lst]
    (match lst
        []  accumulator
        (f (head lst) (foldr f accumulator (tail lst))))))

(foldr + 0 [1 2 3])

;; filter :: (a -> Bool) -> [a] -> [a]
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

(let reverse_ (\[v a]
    (let lazy x (head v))
    (let lazy xs (tail v))
    (let lazy xa (prepend x a))
    (match v
        [] a
        (reverse_ xs xa))))

(let reverse (\[v]
    (reverse_ v [])))

(reverse [1 2 3])

(let compose (\[f g]
    (\[x] (f (g x)))))

((compose (+ 1) (+ 2)) 3)
