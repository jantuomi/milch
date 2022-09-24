(sum2 1 (
    sum2 2 3
)) ; test1
;; (sum2   ; test2
;;     1
;;     2  )
;; "string with space"
;; "another \n\"string\""
;; (let id
;;     (\[a] a))
(\[x] (\[y] (sum2 x y)))