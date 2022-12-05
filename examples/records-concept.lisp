; Records are named tuples with an identifier. `record!` is a builtin function
; that generates functions for creating, reading and modifying a record.
; In type system terms, records are intersections.

; `record!` can only be called in the top-level context.

(record! Ns/User
    name
    phone-number)

; Generates the following functions:

(let! user
    (Ns/User/create "John" "010-123-456"))
    ; user ~> (Ns/User name:"John" phone-number:"010-123-456")

(Ns/User/get-name user) ; => "John"
(Ns/User/get-phone-number user) ; => "010-123-456"

(Ns/User/set-name "Rick" user)
    ; => (Ns/User name:"Rick" phone-number:"010-123-456")
(Ns/User/set-phone-number unit user)
    ; => (Ns/User name:"Rick" phone-number:<unit>)




; Unions are unions of records with an identifier. `union!` is a builtin function
; that generates functions for manipulating the records in the union.

; `union!` can only be called in the top-level context.

(union! Ns/Maybe
    (just value)
    (nothing))

; Generates the following functions:

(let! m1 (Ns/Maybe/just 10)) ; m1 ~> (Ns/Maybe/Just value:10)
(let! m2 (Ns/Maybe/nothing)) ; m2 ~> (Ns/Maybe/Nothing)

(Ns/Maybe/Just/get-value m1) ; => 10




; The builtin function `kind` returns the identifier as a string for any record value

(kind m1) ; => "Ns/Maybe/Just"
(kind m2) ; => "Ns/Maybe/Nothing"
(kind user) ; => "Ns/User"
