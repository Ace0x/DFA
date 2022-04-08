#lang racket

(require racket/trace)

(provide arithmetic-lexer)

; Structure that describes a Deterministic Finite Automaton
(struct dfa-str (initial-state accept-states transitions))

(define (arithmetic-lexer input-string)
  " Entry point for the lexer "
  (automaton-2 (dfa-str 'start '(int var real) delta-arithmetic-1) input-string))

(define (automaton-2 dfa input-string)
  " Evaluate a string to validate or not according to a DFA.
  Return a list of the tokens found"
  (let loop
    ([state (dfa-str-initial-state dfa)]    ; Current state
     [chars (string->list input-string)]    ; List of characters
     [result null])
     ;[element null])                         ; List of tokens found                             ; List of values
    (if (empty? chars)
      ; Check that the final state is in the accept states list
      ;(member state (dfa-str-accept-states dfa))
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
          (if token 
            (cons token result) 
            result))
            ))))


(define (operator? char)
  (member char '(#\+ #\- #\* #\/ #\^ #\=)))

(define (sign? char)
  (member char '(#\+ #\-)))

(define (delta-arithmetic-1 state character)
  " Transition to identify basic arithmetic operations "
  (case state
    ['start (cond
              [(char-numeric? character) (values #f 'int null)]
              [(sign? character) (values #f 'n_sign)]
              [(char-alphabetic? character) (values #f 'var)]
              [(eq? character #\ )(values #f 'o_sp)]
              [(eq? character #\() (values #f 'parenth)]
              [else 'fail])]
    ['n_sign (cond
              [(char-numeric? character) (values #f 'int)]
              [(char-alphabetic? character) (values #f 'var)]
              [ (eq? character #\() (values #f 'parenth)]
              [else (values #f 'fail)])]
    ['int (cond
              [(char-numeric? character) (values #f 'int)]
              [(operator? character) (values 'int 'op)]
              [(or (eq? character #\E) (eq? character #\e)) (values #f 'n_sc)]
              [(eq? character #\ )(values 'int 'n_sp)]
              [(eq? character #\. )(values #f 'dot)]
              [(eq? character #\)) (values 'int  'parenth)]
              [else (values #f 'fail)])]
     ['real (cond
              [(char-numeric? character) (values #f 'real)]
              [(operator? character) (values 'real 'op)]
              [(eq? character #\ )(values 'real 'n_sp)]
              [(eq? character #\)) (values 'real  'parenth)]
              [else (values #f 'fail)])]
    ['dot (cond
              [(char-numeric? character)(values #f 'real)]
              [else (values #f 'fail)])]
    ['n_sc (cond
             [(char-numeric? character) (values #f 'real)]
             [(sign? character) (values #f 'r_sign)]
             [else (values #f 'fail)])]
    ['r_sign (cond
              [(char-numeric? character) (values #f 'real)]
              [else (values #f 'fail)])]
    ['var (cond
              [(char-alphabetic? character) (values #f 'var)]
              [(char-numeric? character) (values #f 'var)]
              [(operator? character) (values 'var 'op)]
              [(eq? character #\_) (values #f 'var)]
              [(eq? character #\ )(values 'var 'n_sp)]
              [(eq? character #\)) (values 'var 'parenth)]
              [else (values #f 'fail)])]
    ['op (cond
              [(char-numeric? character) (values 'op 'int)]
              [(sign? character) (values 'op 'n_sign)]
              [(char-alphabetic? character) (values 'op 'var)]
              [(eq? character #\ ) (values 'op 'o_sp)]
              [(eq? character #\() (values 'op 'parenth)]
              [else (values #f 'fail)])]
    ['o_sp (cond
             [(sign? character) (values #f 'n_sign)]
             [(char-numeric? character) (values #f 'int)]
             [(char-alphabetic? character) (values #f 'var)]
             [(eq? character #\ )(values #f 'o_sp)]
             [(eq? character #\() (values #f 'parenth)]
             [else (values #f 'fail)])]
    ['n_sp (cond
             [(operator? character) (values #f 'op)]
             [(eq? character #\ ) (values #f 'n_sp)]
             [(eq? character #\)) (values #f 'parenth)]
             [else (values #f 'fail)])]
    ['parenth (cond
                [(sign? character) (values 'parenth 'n_sign)]
                [(operator? character) (values 'parenth 'op)]
                [(eq? character #\ ) (values 'parenth 'n_sp)]
                [(char-numeric? character) (values 'parenth 'int)]
                [(char-alphabetic? character) (values 'parenth 'var)]
                [else (values #f 'fail)])]

    ['fail (values #f 'fail)]))

    (automaton-2 (dfa-str 'start '(int real parenth) delta-arithmetic-1) "+(34.2)  +  +955e-24   -  -var * 23.2)")

    