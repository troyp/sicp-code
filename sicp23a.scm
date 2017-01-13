; -------------------------------------------------------------------------------------------------------
; Section 2.3: SYMBOLIC DATA
; --------------------------
; To represent symbolic data we need the ability to "quote" a data object - to denote the name rather than
; the value it is associated with. This requires a new syntactic form (quote obj) which is normally written
; in the shortened form 'obj. Note that since Scheme syntax uses whitespace and parentheses to delimit
; objects, we don't need a closing quote - the single quote simply quotes the next object,
; note: the introduction of quotation prevents a simple substitution method of evaluating expressions
; note: quotation allows us to type in compound objects using the conventional printed notation for sequences
;       eg, '((1 (2 3)) (((4) (5 6)))) - 2 item list of lists; or (1 (2 3) . 3) - 3-item improper list 
;       containing 2 numbers and 1 (proper) list; also. we can now denote the empty list by '()
; The primitive eq? can be used to test 2 symbols for identity.

; memq - takes a symbol and a list as args. If the symbol is not an element of the list, it returns #f.
;        if the symbol is an element, memq returns the sublist beginning with the first occurrence of the symbol.
(define (memq item list)
  (cond ((null? list) #f)
        ((eq? (car list) item) list)
        (else (memq item (cdr list)))))

; [ex 2.53]
; (list 'a 'b 'c)                          =>     (a b c)
; (list (list 'george))                    =>     ((george))
; (cdr '((x1 x2) (y1 y2)))                 =>     ((y1 y2))
; (cadr '((x1 x2) (y1 y2)))                =>     (y1 y2)
; (pair? (car '(a short list)))            =>     #f  ;  the car would return symbol a which is not a pair
; (memq 'red '((red shoes) (blue socks)))  =>     #f  ; red is not an element of the top-level list

; equal? - 2 lists are equal if they have the same structure and elements.
; We can define equal recursively for list thus: 2 symbols are equal? if they are eq?
; 2 lists are equal? if their cars are equal? and their cdrs are equal?
; [ex 2.54] recursive implementation of equal?
(define (newequal? a b)
  (if (not (and (pair? a) (pair? b)))
      (eq? a b)
      (and (newequal? (car a) (car b))
           (newequal? (cdr a) (cdr b)))))

; [ez 2.54] (car ''abracadabra
; the expression 'abracadabra is short for (quote abracadabra)
; ''abracadabra is '(quote abracadabra) ie. a the interpreter sees it as a list of 2 symbols, rather than 
; interpreting the special form. (car ''abracadabra) returns the first of those 2 sqmbols, quote.

; Example 2.3.2: SYMBOLIC DIFFERENTIATION
; ---------------------------------------
; We will use the 'wishful thinking' method of top-down design(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; define new make-sum and make-product procs to simplify expressions involving addition of 0
; or multiplication by 0 or 1
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
; [ex 2.56] add support for exponentiation operations
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
         ((=number? e 1) b)
         ((and (number? b) (number? e)) (expt b e))
         (else (list '** b e))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; [ex 2.57] modify sum and product routines to handle more than 2 terms
; simplest version with no simplification - note: make-sum-from-list allows 0 or more args
(define (make-sum a1 a2 . rest)
  (make-sum-from-list (append (list a1 a2) rest)))
(define (make-sum-from-list terms)
  (cons '+ terms))
(define (augend s)
  (cond ((null? (cdddr s)) (caddr s))
        (else (make-sum-from-list (cddr s)))))

(define (make-product p1 p2 . rest)
  (make-product-from-list (append (list p1 p2) rest)))
(define (make-product-from-list factors)
  (cons '* factors))
(define (multiplicand p)
  (cond ((null? (cdddr p)) (caddr p))
        (else (make-product-from-list (cddr p)))))
; TODO: Better, simplifying, versions.

; [ex 2.58] Redefine predicates, selectors and constructors to work with ordinary infix notation.
; [a] Assume full parenthesization, binary operations.