; lowercase types: polymorphic
; everything else: monomorphic

(type compose (\[
    (\[b] c)  ; f
    (\[a] b)] ; g
    (\[a] c)))
(let!
    compose (\[f g] (\[x] (f (g x)))))

((compose (+ 1) (+ 2)) 3)

(type id (\[a] a))
(let! id (\[a] a))

(type mod (\[Int Int] Int))
(let! mod (\[n k]
    (- n (* k (/ n k)))))

(type not (\[Bool] Bool))
(let! not (\[b]
    (match b
        true false
        false true)))

(type is-even (\[Int] Bool))
(let! is-even (\[n]
    (match (mod n 2)
        0 true
        1 false)))

; match cannot be type annotated because its variadic
; but the match above is inferred as this:
(type match (\[a a b a b] b))
