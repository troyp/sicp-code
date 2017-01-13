#lang scheme
(define (error s x) (display "error: ") (display s) (display " ") (display x))
(define (square x) (* x x))

;; **************************************
;; *                                    *
;; *  MULTIPLE REPRESENTATIONS OF DATA. *
;; *                                    *
;; **************************************
;;
;;   ===================================
;;   Representations of Complex Numbers.
;;   ===================================
;; We want four selectors: real-part, imag-part, magnitude & angle
;; and two constructors: make-from-real-imag & make-from-mag-ang
;; which satisfy the following conditions for any complex number z:
;; (1) (make-from-real-imag (real-part z) (imag-part z)) equals z
;; (2) (make-from-mag-ang (magnitude z) (angle z)) equals z
;; Addition and multiplication are implemented using whichever
;; representation is most convenient for that operation.
(define (add-complex z1 z2)
  (gen-make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (gen-make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (gen-make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (gen-make-from-real-imag (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

;;  ==========
;;  TYPE TAGS.
;;  ==========

;; There are two obvious choices for representing complex numbers:
;; using a cons pair to store two real numbers specifying either
;; the rectangulsr or the polar form.
;; We can carry the "principle of least commitment" even further, and
;; maintain both possibilities for representation, by "type tagging"
;; the data to indicate which form it is in.
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;; Using type-tags, we can define predicates which test for a given
;; representation type.
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;;  ===================
;;  GENERIC PROCEDURES.
;;  ===================

;; We can now define generic constructors and selectors which use these
;; predicates to test the type of their arguments and then dispatch to
;; the appropriate representation-specific procedures.
;; -----rectangular procedures-----
;;    - selectors
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) ; atan can optionally take 2 args:
        (real-part-rectangular z)))  ; (atan y x) == (atan (/ y x))
;;    - constructors
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a))
                                 (* r (sin a)))))
;; -----polar procedures-----
;;    - selectors
(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
;;    - constructors
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
;; -----generic procedures-----
;; We can choose either version of the constructors: no testing
;; and dispatch is required. We choose the mose convenient versions
(define (gen-make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (gen-make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
;; generic selectors use dynamic dispatch
(define (gen-real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z)  (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (gen-imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z)  (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (gen-magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z)  (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (gen-angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z)  (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))
;; This system has 2 weaknesses...
;; 1. The generic interface procs must know explicitly about the individual
;;    representations. If we add a new type, we must add a new clause to each.
;; 2. We must guarantee that no two procs in the entire system have the same name.
;; We can sum these weaknesses up by saying that the implementation of generic procs
;; above is not "additive": when a new representation is added, the generic selectors
;; must be modified and all the other representations need to be checked for name
;; conflicts.
;; What if we had hundreds of representations and many selectors?
;; We must modularize further...

;;  =======================================
;;  DATA-DIRECTED PROGRAMMING & ADDITIVITY.
;;  =======================================

;; Notice that the procs above could be arranged in a table, with the selector/operation
;; on one axis and the representation type on the other. The entries in the table would
;; be the representation-specific procedures. Data-driven programming works with such a
;; table directly. The interface is implemented as a proc that looks up the combination
;; of operation X argument-type in the table to find a proc, then applies it to the
;; contents of the argument.
;;---------------------------------------------------------------------
;; quick-and-dirty list implementation of put and get using global "db"
(define db '())
(define (put op type item)
    (set! db (cons (list op type item) db)))
(define (get op type)
  (define (pred elem)
    (and (equal? (car elem) op)
         (equal? (cadr elem) type)))
  (let ((matches (filter pred db)))
    (if (null? matches)
        (error "No method found for type -- GET" (list op type))
        (let ((match (car matches)))
          (caddr match)))))
;;----------------------------------------------------------------------
(define (install-rectangular-package)
  ;; internal procs
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ {square (real-part z)} {square (imag-part z)})))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (install-polar-package)
  ;; internal procs
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; interface to the reat of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; The selectors access the table using a general "operation" procedure...
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY GENERIC"
               (list op type-tags)))))

;; we can now define our generic selectors...
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;; ...and, using get, define constructors...
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; [ex 2.73] data-directed symbolic differentiation.
;; Recall our symbolic differentiation program. We can regard this
;; prog as performing a dispatch on the type of expr to be
;; differentiated. Here, the 'type tag' is the algebraic op symbol
;; (eg '+) and the operation being performed is deriv. We can 
;; transform it into data-directed style by
;; rewriting the basic derivative proc as...
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;;[ex 2.73a] We extract the operator and look up the deriv proc for that type of expression in the
;; global table. We cannot assimilate the predicates since they should work (and return false) for
;; unknown expression types. Also, there would be nothing to be gained by doing so: it would only make
;; the code less understandable.

;;[ex 2.73b] dum and product deriv procs + auxillary installation code
;; TO-DO: refine procedure to simplify results
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eqv? 0 a1) a2)
        ((eqv? 0 a2) a1)
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((eqv? 1 m1) m2)
        ((eqv? 1 m2) m1)
        ((or (eqv? 0 m1) (eqv? 0 m2)) 0)
        (else (list '* m1 m2))))

(define (install-sum-deriv)
  ;; internal defns
  (define (addend args) (car args))
  (define (augend args) (cdr args))
  (define (sum-deriv args var)
    (cond ((null? args) 0)
          ((null? (cdr args)) (deriv (car args) var))
          (else
           (make-sum (deriv (addend args) var)
                     (deriv (cons '+ (augend args)) var)))))
  ;; interface
  (put 'deriv '+ sum-deriv)
  'done)

(define (install-product-deriv)
  ;; internal defns
  (define (multiplier args) (car args))
  (define (multiplicand args) (cdr args))
  (define (product-deriv args var)
    (cond ((null? args) 1)
          ((null? (cdr args)) (deriv (car args) var))
          (else
           (make-sum (make-product (multiplier args)
                                   (deriv (cons '* (multiplicand args)) var))
                     (make-product (deriv (multiplier args) var)
                                   (multiplicand args))))))
  ;; interface
  (put 'deriv '* product-deriv)
  'done)
;; [ex 2.73c] exponentiation
(define (make-expt b e)
  (cond ((and (number? b) (number? e)) (expt b e))
        ((eqv? 1 b) 1)
        ((eqv? 1 e) b)
        (else (list '** b e))))
(define (install-expt-deriv)
  ;; internal defns
  (define (base args) (car args))
  (define (expt args) (cdr args))
  (define (expt-deriv args var)
    (make-product (make-product (expt args)
                                (make-expt (base args) (make-sum (expt args) -1)))
                  (deriv (base args) var)))
  ;; interface
  (put 'deriv '** expt-deriv)
  'done)

;; [ex 2.74] **** TODO ****

;; ================
;; MESSAGE PASSING.
;; ================

;; Data-directed programming handles generic ops by dealing explictly with type-and-operation tables.
;; The style of programming we used above handled this by making each op resonsible for its own dispatching.
;; In effect, this decomposes the op-type table into rows, with each row representing a generic op.
;; An alternative implementation strategy is to decompose into columns, and instead of having "intelligent
;; operations" dispatching on data type, to work with "intelligent data objects" that dispatch on operation
;; names. We can do this by making a data object, such as a rectangular number, represented by a procedure that
;; takes as input the required operation name and performs the operation indicated.

;; In such a scheme, make-from-real-imag could be defined...
(define (make-from-real-imag-mp x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown operation -- MAKE-FROM-REAL-IMAG-MP" op))))
  dispatch)
;; the corresponding apply-generic procedure would just feed the op's name to the data object...
(define (apply-generic-mp op arg) (arg op))
;; example...
(define (real-part-mp z) (z 'real-part))

;; [ex 7.5] make-from-mag-ang in message-passing style
(define (make-from-mag-ang-mp r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown operation -- MAKE-FROM-MAG-ANG-MP" op))))
  dispatch)

;; [ex 7.6] comparison of strategies for multiple representations.
;; adding new types to message-passing-style: we simply write the code for the new type.
;; adding new functions to message-passing style: we must add code for the new function to every type
;;     (of course, we could always add a clause to all types to check a database for functions which
;;      aren't explicitly coded into it. In that case, we could simply register type-specific versions
;;      of the function for each existing type).
;; adding new types to data-directed style: we simply add a new package, registering the type and its
;;     versions of generic functions with the global db
;; adding new functions to data-directed style: we can add a new generic function in a package just as
;;     easily as we can for a new type.
;; adding new types to explicit dispatch style: we must add addtional cases to the bodies of all
;;     generic functions,
;; adding new functios to explicit dispatch style: we can add new generic functions at any time.
(define (install-show-package)
  (define (show-rect z) "rectangular complex number")
  (define (show-polar z) "polar complex number")
  ; interface
  (put 'show '(rectangular) show-rect)
  (put 'show '(polar) show-polar)
  'done)
;; Summary: if new types must be added, we should use message-passing or data-directed style. If new
;; functions must be added, we should use data-directed style or  explicit dispatch.
;; (example of installing a package for a new function, working for multiple types.
