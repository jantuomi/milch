; What color is your function?
; Impure functions are functions that can call both pure and impure functions.
; Pure functions can only call pure functions.
; Impure function names end in an exclamation mark (!) by convention.
; The top-level context is impure but has special rules
;   related to `import!`, `let!`, and other builtins.

; pure function
(let! f (\[x] x))

; impure function
(let! g! (\![x] (print! x)))
