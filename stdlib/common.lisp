(let! compose (\[f g]
    (\[x] (f (g x)))))

(let! id (\[a] a))

(let! mod (\[n k]
    (- n (* k (/ n k)))))

(let! not (\[b]
    (match b
        true false
        false true)))

(let! is-even (\[n]
    (match (mod n 2)
        0 true
        1 false)))

(let! is-odd (compose not is-even))

;; map :: (a -> b) -> [a] -> [b]
(let! map (\[f lst]
    (match lst
        []
            []
        otherwise
            (prepend (f (head lst)) (map f (tail lst))))))

; (map (+ 1) [1 2 3])

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(let! foldr (\[f accumulator lst]
    (match lst
        []
            accumulator
        otherwise
            (f (head lst) (foldr f accumulator (tail lst))))))

;; filter :: (a -> Bool) -> [a] -> [a]
(let! filter (\[pred lst]
    (match lst
        [] []
        otherwise (match (pred (head lst))
            true
                (prepend (head lst) (filter pred (tail lst)))
            false
                (filter pred (tail lst))))))

(let! fibo (\[n]
    (let! lazy fibo-1 (fibo (- n 1)))
    (let! lazy fibo-2 (fibo (- n 2)))
    (match n
        0  0
        1  1
        _  (+ fibo-1 fibo-2))))

(let! reverse_ (\[v a]
    (let! lazy x (head v))
    (let! lazy xs (tail v))
    (let! lazy xa (prepend x a))
    (match v
        [] a
        _  (reverse_ xs xa))))

(let! reverse (\[v]
    (reverse_ v [])))

(let! flow (\[fs] (foldr compose id (reverse fs))))
(let! pipe (\[x fs] ((flow fs) x)))
