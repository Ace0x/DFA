;By Andrew Dunkerley and Juan Muniain
#lang racket

(require racket/trace)

(provide arithmetic-lexer)

; Structure that describes a Deterministic Finite Automaton
(struct dfa-str (initial-state accept-states transitions))

(define (arithmetic-lexer input-string)
  " Entry point for the lexer "
  (automaton-2 (dfa-str 'start '(int var float o_sp n_sp exp par_close) delta-arithmetic-1) input-string)
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
              [(eq? character #\() (values #f 'par_open)]
              [else  (values #f 'fail)])
    ]
    ['n_sign (cond
              [(char-numeric? character) (values #f 'int)]
              [(char-alphabetic? character) (values #f 'var)]
              [ (eq? character #\() (values #f 'par_open)]
              [else (values #f 'fail)])
    ]
    ['int (cond
              [(char-numeric? character) (values #f 'int)]
              [(operator? character) (values 'int 'op)]
              [(or (eq? character #\E) (eq? character #\e)) (values #f 'exp)]
              [(eq? character #\ )(values 'int 'n_sp)]
              [(eq? character #\. )(values #f 'dot)]
              [(eq? character #\)) (values 'int  'par_close)]
              [else (values #f 'fail)])
    ]
    ['float (cond
              [(char-numeric? character) (values #f 'float)]
              [(operator? character) (values 'float 'op)]
              [(or (eq? character #\E) (eq? character #\e)) (values #f 'exp)]
              [(eq? character #\ )(values 'float 'n_sp)]
              [(eq? character #\)) (values 'float  'par_close)]
              [else (values #f 'fail)])
    ]
    ['dot (cond
              [(char-numeric? character)(values #f 'float)]
              [else (values #f 'fail)])
    ]
    ['exp (cond
              [(char-numeric? character) (values #f 'exp)]
              [(sign? character) (values #f 'r_sign)]
              [(operator? character) (values 'exp 'r_sign)]
              [(eq? character #\ ) (values 'exp 'o_sp)]
              [else (values #f 'fail)])
    ]
    ['r_sign (cond
              [(char-numeric? character) (values #f 'exp)]
              [else (values #f 'fail)])
    ]
    ['var (cond
              [(char-alphabetic? character) (values #f 'var)]
              [(char-numeric? character) (values #f 'var)]
              [(operator? character) (values 'var 'op)]
              [(eq? character #\_) (values #f 'var)]
              [(eq? character #\ )(values 'var 'n_sp)]
              [(eq? character #\)) (values 'var 'par_close)]
              [else (values #f 'fail)])     
    ]
    ['op (cond
              [(char-numeric? character) (values 'op 'int)]
              [(operator? character) (values #f 'fail)]
              [(sign? character) (values 'op 'n_sign)]
              [(char-alphabetic? character) (values 'op 'var)]
              [(eq? character #\ ) (values 'op 'o_sp)]
              [(eq? character #\() (values 'op 'par_open)]
              [else (values #f 'fail)])
    ]
    ['o_sp (cond
              [(sign? character) (values #f 'n_sign)]
              [(char-numeric? character) (values #f 'int)]
              [(char-alphabetic? character) (values #f 'var)]
              [(eq? character #\ )(values #f 'o_sp)]
              [(eq? character #\() (values #f 'par_open)]
              [else (values #f 'fail)])
    ]
    ['n_sp (cond
              [(operator? character) (values #f 'op)]
              [(eq? character #\ ) (values #f 'n_sp)]
              [(eq? character #\)) (values #f 'par_close)]
              [else (values #f 'fail)])
    ]
    ['par_close (cond
              [(sign? character) (values 'par_close 'n_sign)]
              [(operator? character) (values 'par_close 'op)]
              [(eq? character #\ ) (values 'par_close 'n_sp)]
              [(char-numeric? character) (values 'par_close 'int)]
              [(char-alphabetic? character) (values 'par_close 'var)]
              [else (values #f 'fail)])
    ]
    ['par_open (cond
              [(sign? character) (values 'par_open 'n_sign)]
              [(operator? character) (values 'par_open 'op)]
              [(eq? character #\ ) (values 'par_open 'n_sp)]
              [(char-numeric? character) (values 'par_open 'int)]
              [(char-alphabetic? character) (values 'par_open 'var)]
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
        " Test function: arithmetic-lexer"
        ; Numerical types
        (check-equal? (arithmetic-lexer "2") '(("2" int)) "Single digit")
        (check-equal? (arithmetic-lexer "261") '(("261" int)) "Multi digit int")
        (check-equal? (arithmetic-lexer "-63") '(("-63" int)) "Negative int")
        (check-equal? (arithmetic-lexer "5.23") '(("5.23" float)) "Single float")
        (check-equal? (arithmetic-lexer "-5.23") '(("-5.23" float)) "Negative float")
        (check-equal? (arithmetic-lexer ".23") #f "Incorrect float")
        (check-equal? (arithmetic-lexer "2.2.3") #f "Incorrect float")
        (check-equal? (arithmetic-lexer "4e8") '(("4e8" exp)) "Exponent int")
        (check-equal? (arithmetic-lexer "4.51e8") '(("4.51e8" exp)) "Exponent float")
        (check-equal? (arithmetic-lexer "-4.51e8") '(("-4.51e8" exp)) "Negative exponent float")

        ; Variables
        (check-equal? (arithmetic-lexer "data") '(("data" var)) "Single variable")
        (check-equal? (arithmetic-lexer "data34") '(("data34" var)) "Single variable")
        (check-equal? (arithmetic-lexer "34data") #f "Incorrect variable")

        (check-equal? (arithmetic-lexer "2+1") '(("2" int) ("+" op) ("1" int)) "Binary operation ints")
        (check-equal? (arithmetic-lexer "/1") #f "Invalid expression")
        (check-equal? (arithmetic-lexer "6 + 4 *+ 1") #f "Invalid expression")
        (check-equal? (arithmetic-lexer "5.2+3") '(("5.2" float) ("+" op) ("3" int)) "Float and int")
        (check-equal? (arithmetic-lexer "5.2+3.7") '(("5.2" float) ("+" op) ("3.7" float)) "Binary operation floats")

        ; Operations with variables
        (check-equal? (arithmetic-lexer "one+two") '(("one" var) ("+" op) ("two" var)) "Binary operation variables")
        (check-equal? (arithmetic-lexer "one+two/45.2") '(("one" var) ("+" op) ("two" var) ("/" op) ("45.2" float)) "Mixed variables numbers")

        ; Spaces between operators
        (check-equal? (arithmetic-lexer "2 + 1") '(("2" int) ("+" op) ("1" int)) "Binary operation with spaces")
        (check-equal? (arithmetic-lexer "6 = 2 + 1") '(("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators with spaces")
        (check-equal? (arithmetic-lexer "one + two / 45.2") '(("one" var) ("+" op) ("two" var) ("/" op) ("45.2" float)) "Mixed variables numbers spaces")
        (check-equal? (arithmetic-lexer "97 /6 = 2 + 1") '(("97" int) ("/" op) ("6" int) ("=" op) ("2" int) ("+" op) ("1" int)) "Multiple operators")
        (check-equal? (arithmetic-lexer "7.4 ^3 = 2.0 * 1") '(("7.4" float) ("^" op) ("3" int) ("=" op) ("2.0" float) ("*" op) ("1" int)) "Multiple float operators with spaces")

        ; Parentheses
        ;(check-equal? (arithmetic-lexer "()") '(("(" par_open) (")" par_close)) "Open and close")
        ;(check-equal? (arithmetic-lexer "( )") '(("(" par_open) (")" par_close)) "Open space close")
        (check-equal? (arithmetic-lexer "(45)") '(("(" par_open) ("45" int) (")" par_close)) "Open int close")
        (check-equal? (arithmetic-lexer "( 45 )") '(("(" par_open) ("45" int) (")" par_close)) "Open space int space close")
        (check-equal? (arithmetic-lexer "(4 + 5)") '(("(" par_open) ("4" int) ("+" op) ("5" int) (")" par_close)) "Open expression close")
        (check-equal? (arithmetic-lexer "(4 + 5) * (6 - 3)") '(("(" par_open) ("4" int) ("+" op) ("5" int) (")" par_close) ("*" op) ("(" par_open) ("6" int) ("-" op) ("3" int) (")" par_close)) "Open expression close")

        ; Comments
        ;(check-equal? (arithmetic-lexer "3// this is all") '(("3" int) ("// this is all" comment)) "Variable and comment")
        ;(check-equal? (arithmetic-lexer "3+5 // this is all") '(("3" int) ("+" op) ("5" int) ("// this is all" comment)) "Expression and comment")
        ;(check-equal? (arithmetic-lexer "area = 3.1415 * raduis ^2 // area of a circle") '(("area" var) ("=" op) ("3.1415" float) ("*" op) ("raduis" var) ("^" op) ("2" int) ("// area of a circle" comment)) "Complete expression 1")
        ;(check-equal? (arithmetic-lexer "result = -34.6e10 * previous / 2.0 // made up formula") '(("result" var) ("=" op) ("-34.6e10" exp) ("*" op) ("previous" var) ("/" op) ("2.0" float) ("// made up formula" comment)) "Complete expression 2")
        ;(check-equal? (arithmetic-lexer "cel = (far - 32) * 5 / 9.0 // temperature conversion") '(("cel" var) ("=" op) ("(" par_open) ("far" var) ("-" op) ("32" int) (")" par_close) ("*" op) ("5" int) ("/" op) ("9.0" float) ("// temperature conversion" comment)) "Complete expression 3")

        ; Extreme cases of spaces before or after the expression
        (check-equal? (arithmetic-lexer "  2 + 1") '(("2" int) ("+" op) ("1" int)) "Spaces before")
        (check-equal? (arithmetic-lexer "  2 + 1 ") '(("2" int) ("+" op) ("1" int)) "Spaces before and after")
    ))

(displayln "Testing: arithmetic-lexer")
(run-tests test-arithmetic-lexer)