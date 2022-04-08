#lang racket

(require racket/trace)

(provide arithmetic-lexer)

; Structure that describes a Deterministic Finite Automaton
(struct dfa-str (initial-state accept-states transitions))

(define (arithmetic-lexer input-string)
  " Entry point for the lexer "
  (automaton-2 (dfa-str 'start '(int) delta-arithmetic-1) input-string))

(define (automaton-2 dfa input-string)
  " Evaluate a string to validate or not according to a DFA.
  Return a list of the tokens found"
  (let loop
    ([state (dfa-str-initial-state dfa)]    ; Current state
     [chars (string->list input-string)]    ; List of characters
     [result null])                         ; List of tokens found
    (if (empty? chars)
      ; Check that the final state is in the accept states list
      ; (member state (dfa-str-accept-states dfa))
      (if (member state (dfa-str-accept-states dfa))
        (reverse (cons state result)) #f)
      ; Recursive loop with the new state and the rest of the list
      (let-values
        ; Get the new token found and state by applying the transition function
        ([(token state) ((dfa-str-transitions dfa) state (car chars))])
        (loop
          state
          (cdr chars)
          ; Update the list of tokens found
          (if token (cons token result) result))))))

(define (operator? char)
  (member char '(#\+ #\- #\* #\/ #\^ #\=)))

(define (sign? char)
  (member char '(#\+ #\-)))

(define (parenth? char)
  (member char '(#\( #\))))

(define (delta-arithmetic-1 state character)
  " Transition to identify basic arithmetic operations "
  (case state
    ['start (cond
              [(char-numeric? character) (values #f 'int)] ; digit
              [(char-alphabetic? character) (values #f 'var)] ; var
              [(eq? character #\ )(values #f 'start)] ; space loop
              [(parenth? character)(values #f 'parenth)] ; parenth
              [(sign? character) (values #f 'sign)] ; sign
              [else 'fail])]
    ['sign (cond
              [(char-numeric? character) (values #f 'int)] ; digit
              [(char-alphabetic? character) (values #f 'var)] ; var
              [(eq? character #\ )(values #f 'start)] ; space loop
              [(parenth? character)(values #f 'parenth)] ; parenth
              [else (values #f 'fail)])]
    ['int (cond
              [(char-numeric? character) (values #f 'int)] ; digit loop
              [(operator? character) (values 'int 'op)] ; op
              [(or (eq? character #\E) (eq? character #\e)) (values #f 'real)]
              [(eq? character #\ )(values 'int 'space)] ; space
              [(eq? character #\. )(values #f 'dot)]  ; dot
              [(parenth? character)(values #f 'parenth)] ; parenth
              [else (values #f 'fail)])]
    ['E_state (cond
              )]
    ['dot (cond
              [(char-numeric? character)(values #f 'real)] ; digit to real
              [(eq? character #\ )(values 'real 'space)] ; space
              [else (values #f 'fail)])]
    ['space (cond
             [(operator? character) (values #f 'op)] ; op
             [(eq? character #\ )(values #f 'space)] ; space loop
             [else (values #f 'fail)])]
    ['var (cond
              [(char-alphabetic? character) (values #f 'var)] ; var
              [(char-numeric? character) (values #f 'var)] ; digit
              [(operator? character) (values 'var 'op)] ; op
              [(eq? character #\_) (values #f 'var)] ; underscore
              [(eq? character #\ )(values 'var 'space)] ; space
              [else (values #f 'fail)])]
    ['op (cond
              [(char-numeric? character) (values 'op 'int)] ; digit
              [(sign? character) (values 'op 'sign)] ; sign
              [(char-alphabetic? character) (values 'op 'var)] ; var
              [(eq? character #\ ) (values 'op 'op)] ; space loop
              [(parenth? character)(values #f 'parenth)] ; parenth
              [else (values #f 'fail)])]
    ['parenth (cond
             [(sign? character) (values 'parenth 'sign)]
             [(char-numeric? character) (values 'parenth 'int)]
             [(char-alphabetic? character) (values 'parenth 'var)]
             [(eq? character #\ )(values #f 'parenth)] ; space loop
             [(parenth? character)(values 'parenth 'parenth)] ; parenth
             [else (values #f 'fail)])]
    ['real (cond
             [(operator? character) (values 'real 'op)] ; op
             [(eq? character #\ ) (values 'real 'space)] ; space
             [(parenth? character)(values 'real 'parenth)] ; parenth
             [(char-numeric? character) (values #f 'real)] ; digit loop
             [else (values #f 'fail)])]
    
    ['fail (values #f 'fail)]))

    (arithmetic-lexer "+34.2+999E-4")

    (automaton-2 (dfa-str 'start '(int) delta-arithmetic-1) "34+9-var*23")