;By Andrew Dunkerley and Juan Muniain
#lang racket

(require racket/trace)

(provide arithmetic-lexer)

; Structure that describes a Deterministic Finite Automaton
(struct dfa-str (initial-state accept-states transitions))

(define (arithmetic-lexer input-string)
  " Entry point for the lexer "
  (automaton-2 (dfa-str 'start '(int var float o_sp n_sp) delta-arithmetic-1) input-string)
)

(define (automaton-2 dfa input-string)
  " Evaluate a string to validate or not according to a DFA.
  Return a list of the tokens found"
  (let loop
    ([state (dfa-str-initial-state dfa)]    ; Current state
     [chars (remove* (list #\space " ") (string->list input-string))]    ; List of characters
     [element null]
     [result null])
    (if (empty? chars)
      ; Check that the final state is in the accept states list
      ; (member state (dfa-str-accept-states dfa))
      (if (member state (dfa-str-accept-states dfa))
        (reverse (cons (list (list->string (reverse element))state) result)) #f)
      ; Recursive loop with the new state and the rest of the list
      (let-values
        ; Get the new token found and state by applying the transition function
        ([(token state) ((dfa-str-transitions dfa) state (car chars))])
        (loop
          state
          (cdr chars)
          (cond
            [token (if false null (list(first chars)))] ;redundant since we removed white spaces
            [(first chars) (cons (first chars) element)] ;redundant since we removed white spaces
          )
          (if token (cons (list (list->string (reverse element)) token) result) result)
        )
      )
    )
  )
)

(define (operator? char)
  (member char '(#\+ #\- #\* #\/ #\^ #\=))
)

(define (sign? char)
  (member char '(#\+ #\-))
)

(define (delta-arithmetic-1 state character)
  " Transition to identify basic arithmetic operations "
  (case state
    ['start (cond
              [(char-numeric? character) (values #f 'int)]
              [(sign? character) (values #f 'n_sign)]
              [(char-alphabetic? character) (values #f 'var)]
              [(eq? character #\ )(values #f 'o_sp)]
              [(eq? character #\() (values #f 'parenth)]
              [else 'fail])
    ]
    ['n_sign (cond
              [(char-numeric? character) (values #f 'int)]
              [(char-alphabetic? character) (values #f 'var)]
              [ (eq? character #\() (values #f 'parenth)]
              [else (values #f 'fail)])
    ]
    ['int (cond
              [(char-numeric? character) (values #f 'int)]
              [(operator? character) (values 'int 'op)]
              [(or (eq? character #\E) (eq? character #\e)) (values #f 'n_sc)]
              [(eq? character #\ )(values 'int 'n_sp)]
              [(eq? character #\. )(values #f 'dot)]
              [(eq? character #\)) (values 'int  'parenth)]
              [else (values #f 'fail)])
    ]
    ['float (cond
              [(char-numeric? character) (values #f 'float)]
              [(operator? character) (values 'float 'op)]
              [(eq? character #\ )(values 'float 'n_sp)]
              [(eq? character #\)) (values 'float  'parenth)]
              [else (values #f 'fail)])
    ]
    ['dot (cond
              [(char-numeric? character)(values #f 'float)]
              [else (values #f 'fail)])
    ]
    ['n_sc (cond
              [(char-numeric? character) (values #f 'float)]
              [(sign? character) (values #f 'r_sign)]
              [else (values #f 'fail)])
    ]
    ['r_sign (cond
              [(char-numeric? character) (values #f 'float)]
              [else (values #f 'fail)])
    ]
    ['var (cond
              [(char-alphabetic? character) (values #f 'var)]
              [(char-numeric? character) (values #f 'var)]
              [(operator? character) (values 'var 'op)]
              [(eq? character #\_) (values #f 'var)]
              [(eq? character #\ )(values 'var 'n_sp)]
              [(eq? character #\)) (values 'var 'parenth)]
              [else (values #f 'fail)])     
    ]
    ['op (cond
              [(char-numeric? character) (values 'op 'int)]
              [(sign? character) (values 'op 'n_sign)]
              [(char-alphabetic? character) (values 'op 'var)]
              [(eq? character #\ ) (values 'op 'o_sp)]
              [(eq? character #\() (values 'op 'parenth)]
              [else (values #f 'fail)])
    ]
    ['o_sp (cond
              [(sign? character) (values #f 'n_sign)]
              [(char-numeric? character) (values #f 'int)]
              [(char-alphabetic? character) (values #f 'var)]
              [(eq? character #\ )(values #f 'o_sp)]
              [(eq? character #\() (values #f 'parenth)]
              [else (values #f 'fail)])
    ]
    ['n_sp (cond
              [(operator? character) (values #f 'op)]
              [(eq? character #\ ) (values #f 'n_sp)]
              [(eq? character #\)) (values #f 'parenth)]
              [else (values #f 'fail)])
    ]
    ['parenth (cond
              [(sign? character) (values 'parenth 'sign)]
              [(operator? character) (values 'parenth 'op)]
              [(eq? character #\ ) (values 'parenth 'n_sp)]
              [(char-numeric? character) (values 'parenth 'int)]
              [(char-alphabetic? character) (values 'parenth 'var)]
              [else (values #f 'fail)])
    ]
    ['fail (values #f 'fail)]
  )
)

; Import library for unit testing
(require rackunit)
; Import necesary to view the test results
(require rackunit/text-ui)

(define test-arithmetic-lexer
  (test-suite
    " Test function: arithmetic-lexer "
    (check-equal? (arithmetic-lexer "6 = 2 + 1") '(("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators with spaces")
    (check-equal? (arithmetic-lexer "97 /6 = 2 + 1") '(("97" int) ("/" op) ("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators")
    (check-equal? (arithmetic-lexer "7.4 ^3 = 2.0 * 1") '(("7.4" float) ("^" op) ("3" int) ("=" op) ("2.0" float) ("*" op) ("1" int)) "Multiple float operators with spaces")
    (check-equal? (arithmetic-lexer "  2 + 1") '(("2" int) ("+" op) ("1" int)) "Spaces before")
    (check-equal? (arithmetic-lexer "  2 + 1 ") '(("2" int) ("+" op) ("1" int)) "Spaces before and after")
    (check-equal? (arithmetic-lexer "2+1") '(("2" int) ("+" op) ("1" int)) "Binary operation ints")
    (check-equal? (arithmetic-lexer "5.2+3") '(("5.2" float) ("+" op) ("3" int)) "Float and int")
    (check-equal? (arithmetic-lexer "5.2+3.7") '(("5.2" float) ("+" op) ("3.7" float)) "Binary operation floats")
    (check-equal? (arithmetic-lexer "one+two") '(("one" var) ("+" op) ("two" var)) "Binary operation variables")
    (check-equal? (arithmetic-lexer "2 + 1") '(("2" int) ("+" op) ("1" int)) "Binary operation with spaces")
    (check-equal? (arithmetic-lexer "6 = 2 + 1") '(("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators with spaces")
    (check-equal? (arithmetic-lexer "97 /6 = 2 + 1") '(("97" int) ("/" op) ("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators")
    (check-equal? (arithmetic-lexer "7.4 ^3 = 2.0 * 1") '(("7.4" float) ("^" op) ("3" int) ("=" op) ("2.0" float) ("*" op) ("1" int)) "Multiple float operators with spaces")
    (check-equal? (arithmetic-lexer "3// this is all") '(("3" int) ("// this is all" comment)) "Variable and comment")
    (check-equal? (arithmetic-lexer "3+5 // this is all") '(("3" int) ("+" op) ("5" int) ("// this is all" comment)) "Expression and comment")
  )
)

(displayln "Testing: arithmetic-lexer")
(run-tests test-arithmetic-lexer)