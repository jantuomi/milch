; FUNCTIONS

(let prompt-input (\[]
    (let p "> ")
    (do get-user-input "prompted" p)))

; SIGNAL HANDLERS

(on user-input (\[msg s]
    (match msg
        "prompted" (batch [
            (do print-line (fmt "You entered: %%" [s]))
            (prompt-input)])
        (do fatal-error (fmt "error: unknown signal %%" [msg])))))

; ENTRYPOINT

(prompt-input)
