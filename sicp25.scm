#lang racket

;;------------------------------------------------------------------------
;; utility procs
(define (error s x) (display "error: ") (display s) (display " ") (display x))
(define (square x) (* x x))
(define pi 3.14159)
(define (id x) x)
;; TODO: This is buggy - FIX
;; (find-elem (lambda (x) (not (pair? (car x)))) '(({2 3} 5) ((3)) (9 (6 5)) 3))   => (9 (6 5))
;; (find-elem (lambda (x) (not (pair? (car x)))) '(({2 3} 5) ((3)) . (9 (6 5))))   => error
;; "car: expects argument of type <pair>; given 9".  Note: improper list ok with int elements.
(define (find-elem pred ls)
  (if (not (pair? ls)) (error "not pair." ls)
  (if (null? ls) #f
      (let ((fst (car ls))
            (snd (cdr ls)))
        (if (pred fst) fst
            (cond ((pair? snd) (find-elem pred snd))
                  ((pred snd) snd)
                  (else #f)))))))
(define (inq? elem ls)
  (if (null? ls) #f
  (if (eq? elem (car ls))
      #t
      (inq? elem (cdr ls)))))
;;----------------------------------------------------------------------
;; Type Tag constructor & selectors (replaced in ex 2.78)
;(define (attach-tag type-tag contents) (cons type-tag contents))
;(define (type-tag datum)
;  (if (pair? datum)
;      (car datum)
;      (error "Bad tagged datum -- TYPE-TAG" datum)))
;(define (contents datum)
;  (if (pair? datum)
;      (cdr datum)
;      (error "Bad tagged datum -- CONTENTS" datum)))
;;---------------------------------------------------------------------
;; quick-and-dirty list implementation of put and get using global "db"
(define db '())
(define (generic-procs) db)  ; returns the generic procedure table in the form of a
;; list of entries. The procs get-name, etc, can be used on an entry to extract info
;; accessors for accsesing the fields of an entry...
(define (get-name listof3) (car listof3))
(define (get-type listof3) (cadr listof3))
(define (get-proc listof3) (caddr listof3))
(define (put op type item)
    (set! db (cons (list op type item) db)))
(define (get op type)
  (define (pred elem)
    (and (equal? (get-name elem) op)
         (equal? (get-type elem) type)))
  (let ((matches (filter pred db)))
    (if (null? matches)
        #f
        (let ((match (car matches)))
          (caddr match)))))

;;----------------------------------------------------------------------
;; apply-generic
(define (apply-generic-ORIG op . args)    ; new apply-generic: see below, "Coercion"
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY GENERIC"
               (list op type-tags)))))
;;---------------------------------------------------------------------
;; rectangular and polar packages
(define (install-rectangular-package)
  ;; internal procs
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
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
  (define (make-from-mag-ang r a)
    (cond ((zero? a) (cons 0 0))
          ((<= a (- pi)) (make-from-mag-ang r (+ a (* 2 pi))))
          ((> a pi) (make-from-mag-ang r (- a (* 2 pi))))
          (else (cons r a))))
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
;; -----------------------------------------------------------
;; Generic selectors for complex numbers
;; NOTE: these override builtin procs for primitive complex type
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;; Generic constructors for rectangular and polar representations
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
;; ------------------------------------------------------------


;; *********************************************
;; *                                           *
;; *  CH 2.5: SYSTEMS WITH GENERIC OPERATIONS  *
;; *                                           *
;; *********************************************

;; We will design a generic arithmetic system capable of operating
;; on different types using additive packages. Any given type may
;; itself be accessed by generic procedures that dispatch according
;; to representation

;; We implement the generic arithmetic operators using the same
;; strategy we used for generic selectors for complex numbers: we'll
;; attach a type-tag to each kind of number and have the generic proc
;; dispatch according to the data type or its args.
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;; We first install a package for dealing with ordinary numbers, ie.
;; the primitive numbers of the language. We'll tag these with
;; 'scheme-number and dispatch to the primitive aritmetic procs. Since
;; each op takes 2 args, they are installed in the table keyed by the
;; list (scheme-number scheme-number)
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  'done)
;; Users of the scheme-number package will create (tagged) ordinary
;; numbers with the proc...
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; With the framework in place, we can add new kinds of numbers. Here
;; is a rationals package. Note: thanks to additivity, we can use our
;; previous rational code as the internal procs.
(define (install-rational-package)
  ;; internal procs
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
  (define (zero-rat? x)
    (=zero? (numer x)))
  (define (negate-rational x)
    (make-rat (neg (numer x)) (denom x)))
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put '=zero? '(rational)
       (lambda (x) (zero-rat? x)))
  (put 'neg '(rational)
       (lambda (x) (tag (negate-rational x))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; We can install a package for complex numbers, extracting from the
;; table the ops make-from-real-imag and make-from-mag-ang, that were
;; defined by the rectangular and polar packages. Additivity allows
;; us to use the same internal ops from sect2.4
;; Since the complex package uses rectangular and polar procs as
;; convenient, both packages are required to support the arithmetic ops.
(define (install-complex-package)
  ;; imported procs from rect and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'polar) x y))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (zero-complex? z)
    (=zero? (magnitude z)))
  (define (negate-complex z)
    (make-from-real-imag (neg (real-part z))
                         (neg (imag-part z))))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put '=zero? '(complex) zero-complex?)
  (put 'neg '(complex) (lambda (z) (tag (negate-complex z))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-angle r a)
  ((get 'make-from-real-imag 'complex) r a))

;; We have here a 2-level tag system. A complex number,
;; say 3+4i, in rectangular form is represented by
;; '(complex rectangular 3 . 4). The outer tag (complex)
;; is used to direct the number to the complex package.
;; Within the complex package, the next tag (rectangular)
;; is used to direct the number to the rectangular package.
         
;; In a larger and more complicated system, there may be
;; many levels, each interfaced to the next by means of
;; generic operations. As a data object is passed "downwards",
;; the type-tag is used to dispatch it to the appropriate
;; package. At the same time, the tag is stripped off
;; (using 'contents') and the next level of tag (if any)
;; becomes visible to be used for further dispatching.

;; Note: while we use specific names like "add-complex"
;; for the type-specific procs within a package, there is
;; no need to make them distinct. We could just as easy use
;; "add" for the addition proc in every package, since
;; these procs are internal to the package.

;; [ex 2.77] To add a real-part operation fo the complex type,
;; all we had to do was add
;;           (put 'real-part 'complex real-part) to the
;; package. Why does this work? There was already a real-part
;; generic procedure defined: it uses the type-tag of the arg
;; to search for an appropriate 'real-part entry in the table.
;; There was no such entry for complex, but there were entries
;; for 'rectangular and 'polar. A number of type complex is a
;; rectangular or polar number with a 'complex tag at the head.
;; All we need do to find the real-part is strip off the
;; 'complex tag and then find the real-part of the resulting
;; rectangular or polar number. This is exactly what
;; (apply-generic real-part z) does, and therefore what
;; (real-part z) does. (real-part z) strips off the complex tag,
;; looks up the table for the type-specific op, is told to apply
;; real-part again. It then strips off the rectangular or polar
;; tag and looks up the table again, being sent to the appropriate
;; package for the specific operation.

;; [ex 2.78] We have used scheme's primitive numbers wrapped in
;;           a type-tag. However, scheme already has internal
;;           types, which can be detected with predicates like
;;           number? and symbol?. Modify the type-tag constructor
;;           and selectors to take advantage of this, so ordinary
;;           numbers should be represented simply as scheme numbers
;;           rather than pairs (scheme-number, n).
;; I'll define utils (lowest-level): put-tag, get-tag, detag, type-obj?
(define (put-tag typeTag contents)
  (if (eq? typeTag 'scheme-number)
      contents
      (cons typeTag contents)))
(define (get-tag obj)
  (if (number? obj)
      'scheme-number
      (car obj)))
(define (detag obj)
  (if (number? obj)
      obj
      (cdr obj)))
(define (typed-obj? obj)
  (or (number? obj) (pair? obj)))
;; Now, replace cons, car, cdr and pair in the constructors/selectors
;; with these procs...
(define (attach-tag type-tag contents) (put-tag type-tag contents))
(define (type-tag datum)
  (if (typed-obj? datum)
      (get-tag datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (typed-obj? datum)
      (detag datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; [ex 2.79] Define a generic equality predicate equ? that works
;; with ordinary, rational and complex numbers.
(define (equ? num1 num2)
  (cond ((and (number? num1) (number? num2))
         (cond ((and (exact? num1) (inexact? num2)) (eqv? (exact->inexact num1) num2))
               ((and (inexact? num1) (exact? num2)) (eqv? num1 (exact->inexact num2)))
               (else (eqv? num1 num2))))
        ; base case (note: "or" slightly more efficient than "and")
        ((not (and (cons? num1) (cons? num2))) (eq? num1 num2))
        ((eqv? (type-tag num1) (type-tag num2))
         (equ? (contents num1) (contents num2)))
        (else #f)))
;; [this works reasonable with rational/complex nums as it recursively checks type-tags and
;;  contents, and interprets the first number in a pair as the "type-tag" and the second as
;;  "contents".]
;; note: This will never judge numbers of different types to be equal.
;; If we wish to judge equality of, say, different representations of
;; complex numbers, we should write a predicate that uses apply-generic
;; to dispatch to type-specific versions. note: Also, this may fail if
;; a type has more than one representation for the same number (eg.,
;; rational, polar). make-rat automatically puts numbers into lowest
;; form so there is no problem. We need to alter the polar package to
;; make sure polar numbers are in a "normal form". This means there
;; will be a restriction on all future number packages, that the
;; representations they generate correspond uniquely to numbers, even
;; if the input representations are not.
;; There is a way to lift this limitation: we could add the option for
;; a package to include its own equality predicate. That way, equ? could
;; dispatch to the specific predicate if it exists, or otherwise fall
;; back on its default procedure. This would allow for non-1-1
;; representations. However, it doesn;t seem necessary at the moment,
;; and this option can be easily added later.

;; [ex 2.80] Define a generic predicate =zero? working for ordinary,
;; rational and complex nums
;(define (=zero? num)
;  (equ? (add num num) (sub num num)))
;; There is a problem with (equ? num (sub num num)) where subtraction
;; changes the representation of a number (eg. polar to rectangular).
;; There is a safe chance that sub and add reult in the same type.


;; ==================================
;; COMBINING DATA OF DIFFERENT TYPES.
;; ==================================

;; So far we've treated the different data types as completely
;; independent. However, it is sometimes meaningful to define
;; operations across type boundaries, eg. adding a scheme-number to a
;; complex number. One way to handle this is to design a different
;; proc for each type combination and install these procs in the table .
;; eg we could add the following to the complex package...
;(define (add-complex-to-schemenum z x)
;  (make-from-real-imag (+ (real-part z) x)
;                       (imag-part z)))
;(put 'add '(complex scheme-number)
;     (lambda (z x) (tag (add-complex-to-schemenum z x))))
;; This technique works, but is cumbersome. As the number of types
;; increase, there is a rapid increase in install costs if cross-type
;; compatiblity is to be maintained. Also, there is the question of
;; how to package the procedures appropriately: which package should
;; have responsibility for which cross-type ops? This method undermines
;; our ability to combine packages additively.
;; In the most general case of completely unrelated types, explicit
;; cross-type operations are our only option. Luckily, we can usually do
;; better by taking advantage of additional structure latent in our type
;; system.

;; Coercion.
;; ---------

(define coerce-db '())
(define (put-coercion type1 type2 proc)
  (set! coerce-db (cons (cons (cons type1 type2) proc) coerce-db)))
(define (get-coercion type1 type2)
  (define (pred elem)
    (equal? (car elem) (cons type1 type2)))
  (let ((matches (filter pred coerce-db)))
    (if (null? matches)
        #f
        (cdar matches))))
(define (coercion-procs) coerce-db) ; coercion-procs returns the table of coercion 
;; procs as a list of entries ((t1 . t2) . op), in case of reimplementation.
(define (get-from-type entry) (caar entry))
(define (get-to-type entry) (cdar entry))
(define (get-coercion-proc entry) (cdr entry))

;; Often, the types involved are not completely independent, and
;; there may be ways in which an object of one type may be viewed
;; as an object of another type (eg. we can view an ordinary number
;; as a complex number with imaginary part zero). This process is
;; called "coercion". We can implement this idea by designing
;; coercion procs which transforms one type of object into another,
;; and install these procs in a special coercion table, indexed
;; under the names of the two types. Note that some of the slots
;; in the table will be empty since it's not possible in general
;; to convert one arbitrary type into another.

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (rational->scheme-number r)
  (let* ((ratio (contents r))
         (numer (car ratio))
         (denom (cdr ratio)))
    (make-scheme-number (/ numer denom))))
(put-coercion 'rational 'scheme-number rational->scheme-number)

;; Once we have the coercion table set up, we can handle coercion
;; in a uniform manner by modifying "apply-generic": When asked to
;; perform an op, we first check whether the op is defined for the
;; types (same as before) - if so, we dispatch to the op in the
;; operations-and-type table. Otherwise, we attempt coercion. First
;; (assuming 2 args), we try to coerce the first arg into the second
;; arg's type. If we can't, we try to coerce the second arg into the
;; type of the first. If neither can be coerced to the other type,
;; we give up.
;;----------------------------------------------------------------------
;; apply-generic - with coercion
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err)
      (error "No method for these types"
             (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (and (= (length args) 2)
                 (not (eq? (car type-tags) (cadr type-tags))))
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                      (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                      (else (err)))))
            (err)))))
;;---------------------------------------------------------------------
;; However, there may be cases where our coercion scheme is insufficient.
;; What if neither of the objects to be combined can be converted to the type of the
;; other, but both can be converted to a third type? We may wish to exploit this
;; possibility.
;; In order to do this and maintain modularity, we must usually take advantage of
;; still more structure in the relations among types.


;; Heirarchies of Types.
;; ----------------------
;; Above, we relied on relations between pairs of types, but often there is a more
;; "global" structure to type relationships. Consider numeric types, where we
;; typically have a "heirarchy of types", or "tower" - integers are a subtype of
;; rationals, which are a subtype of reals, etc.
;; When we have a tower structure, we only need to specify how to convert a type to
;; the supertype immediately above it. Two types can then be combined by converting
;; them to their common supremum stepwise (in a simple tower, as opposed to a tree,
;; one type will be converted to the other).
;; We can redesign apply-generic to handle this as follows: for each type in a tower,
;; we supply a "raise" proc to convert it to the type above. When we wish to combine
;; objects, we successivley raise the lower types until all objects are at the same
;; level of the tower.
;; Another advantage of such a tower implementation is that we can easily implement
;; the notion that every type inherits all ops defined for its supertypes. We just
;; modify apply-generic so that if an op isn't defined directly for a type, we raise
;; the type and try again. We thus climb the tower searching for the op until we
;; either reach a type for which it's defined, or reach the top and fail.
;; Another advantage of a tower is that it gives us a simple way to "lower" an object
;; to the simplest representation (eg. 2+3i + 4-3i --> 6 rather than 6+0i).


;; Inadequacies of Heirarchies.
;; ----------------------------
;; In general, relations among types may be more general than a heirarchy.
;; Types may have not only multiple subtypes, but multiple supertypes. So the type
;; relations form a graph rather than a chain or tree.
;; The multiple supertypes issue is problematic, since there is no unique way to
;; raise a type. If we wish to coerce two types to a common third type, considerable
;; searching may be required.
;; Dealing with many interrelated types while maintaining modularity in a large
;; system is very difficult, and is an area of research.

;; [ex 2.81a] Louis notices (apply-generic op x y) may try to coerce x and y to the
;;            each other's type even if they're already of the same type. He adds
;;            "coercion" procedures to "convert" a type to itself (just an identity
;;            function). If we now use apply-generic with operands of the same type
;;            and an op not defined for that type, what happens?
;; An infinite loop ensues. When the initial lookup fails, apply-generic will attempt
;; to coerce the first type to the second, and succeed due to the added procs. The
;; result will be that apply-generic is called again *with the exact same arguments*.
;; Again, it will end up calling itself, ad infinitem.
;; [ex 2.81b] Does apply-generic need to be modified for this case?
;; No. apply-generic works fine with args of the same type. If the op is defined for
;; that type, the first lookup will find it; if not, both the initial lookup and the
;; checks for coercion procs will fail. The only problem is the wasted testing for
;; coercion procs.
;; [ex 2.81c] modify apply-generic so it won't try coercion for args of the same type
;; (see proc above)
;; [ex 2.82] Show how to generalise apply-generic to handle coercion of multiple args.
;;           One strategy is to try to coerce all args to the type of the first arg,
;;           then to the type of the second arg, and so on. Give an example where this
;;           strategy is not sufficiently general.
;; If the args are of types (a,b,c) there may be an op for types (a,b,b) but not for
;; (a,a,a) or (b,b,b). There's also a chance that two types can both be converted to
;; a third type for which an appropriate op may exist (this applies to the 2-arg case
;; as well).
(define (coercion-from? type entry)
  (equal? type (caar entry)))
(define (coercion-to? type entry)
  (equal? type (cdar entry)))
(define (coerce-image type)
  (let ((cdb (coercion-procs))
        (pred (lambda (entry) (coercion-from? type entry))))
    (map car (filter pred cdb))))
(define (generic-preimage name)
  (let ((gdb (generic-procs))
        (pred (lambda (entry) (equal? name (get-name entry)))))
    (map get-type (filter pred gdb))))
(define (find-coercion from to) ; type type -> coercion-db entry
  (if (eq? from to) (cons (cons from to) id)
  (let ((cdb (coercion-procs))
        (pred (lambda (entry) (and (coercion-from? from entry) (coercion-to? to entry)))))
    (let ((success (filter pred cdb)))
      (if (null? success) #f
          (car success))))))
(define (coercion-list to-type typelist)
  (map (lambda (type)
         (let ((entry (find-coercion type to-type)))
           (if entry
               (cdr entry)
               #f)))
       typelist))

(define (convert-to-type type arglist)
  (let* ((typelist (map type-tag arglist))
         (coercelist (coercion-list type typelist)))
    (map (lambda (proc val)
           (if proc
               (proc val)
               #f))
         coercelist
         arglist)))

(define (unify-types arglist)
  (let iter ((types-to-try arglist))
    (cond ((null? types-to-try) #f)
          ((not (pair? types-to-try)) (error "UNIFY-TYPES: arg must be a proper list " arglist))
          (else
           (let ((target-type (type-tag (car types-to-try)))
                 (rest (cdr types-to-try)))
             (let ((result (convert-to-type target-type arglist)))
               (if (nofails? result)
                   result
                   (iter rest))))))))
(define (nofails? ls) (not (memq #f ls)))

;; apply-generic with coercion for multiple args
(define (apply-generic-2 op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err)
      (error "No method for these types"
             (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((converted-args (unify-types args)))
          (if converted-args
              (apply apply-generic (cons op converted-args))
              (err))))))

;; [ex 2.83] Suppose you're designing a generic arithmetic system for the numeric tower
;;           (integer rational real complex).
;;           For each type (except complex), design a "raise" proc that raises an object to the
;;             next higher level.
;;           Show how to install a generic raise op that will work for each type (except complex)

;; integer-package
(define (install-integer-package)
  ;; internal procs
  (define (tag) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (n m) (tag (+ (contents n) (contents m)))))
  (put 'mul '(integer integer)
       (lambda (n m) (tag (* (contents n) (contents m)))))
  (put 'sub '(integer integer)
       (lambda (n m) (tag (- (contents n) (contents m)))))
  (put 'div '(integer integer)
       (lambda (n m) (tag (truncate (/ (contents n) (contents m))))))
  (put 'make 'integer (lambda (n) (tag n)))
  'done)
;; constructor
(define (make-integer n) (attach-tag 'integer n))

;; real-package
(define (install-real-package)
  ;; internal procs
  (define (tag) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (n m) (tag (+ (contents n) (contents m)))))
  (put 'mul '(real real)
       (lambda (n m) (tag (* (contents n) (contents m)))))
  (put 'sub '(real real)
       (lambda (n m) (tag (- (contents n) (contents m)))))
  (put 'div '(real real)
       (lambda (n m) (tag (/ (contents n) (contents m)))))
  (put 'make 'real (lambda (n) (tag n)))
  'done)
;; constructor
(define (make-real n) (attach-tag 'real n))

;; We break encapsulation here, using the internal representation, since the datatype
;; interfaces don't provide easy methods coercion to other types.
(define (integer->rational n)
  (make-rational (contents n) 1))
(define (rational->real r)
  (let* ((ratio (contents r))
         (numer (car ratio))
         (denom (cdr ratio)))
    (make-real (exact->inexact (/ numer denom)))))
(define (real->complex x)
  (make-complex-from-real-imag (contents x) 0))
(put 'raise '(integer) integer->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)
(put 'raise '(complex) (lambda (type) #f))

(define (raise x)
  (and x    ; if x is #f, return #f
       (if (genproc-exists? 'raise (type-tag x))
           (let ((proc (get 'raise (list (type-tag x)))))
             (if proc (proc x) #f))
           #f)))

(define (genproc-exists? name type)
  (define (pred entry)
    (and (eq? (get-name entry) name)
         (eq? (get-type entry) type)))
  (foldr (lambda (a b) (or a b)) #f (generic-procs)))

;; [ex 2.84] Use the "raise" op from ex2.83 to modify apply-generic so that it coerces its args
;;           to the same type by successive raising. Make it compatible with the rest of the
;;           system so that new levels can be added to the tower without problems.
(define (apply-generic-3 op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err)
      (error "No method for these types"
             (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((converted-args (unify-types args)))
          (if converted-args
              (apply apply-generic (cons op converted-args))
              (err))))))
(define (numeric? val)
  (let ((type (type-tag val)))
    (inq? type
          (list 'scheme-number 'rational 'real 'complex))))
(define (type> val1 val2)
  (if (raise-to val2 (type-tag val1)) #t #f))
(define (raise-to val type)
  (define (iter v)
    (if (eq? v #f) #f
    (let ((t (type-tag v)))
      ; (display (list v t)) (newline)
      (if (eq? t type)
          v
          (iter (raise v))))))
  (iter val))
(define (type= val1 val2)
  (eq? (type-tag val1) (type-tag val2)))
(define (raise/unify arglist)
  (if (< (length arglist) 2)
      arglist
      (let ((fst (car arglist))
            (snd (cadr arglist))
            (rest (cddr arglist)))
        (cond ((type> fst snd) (cons fst
                                     (raise/unify (cons (raise-to snd (type-tag fst))
                                                        rest))))
              ((type> snd fst) (cons (raise-to fst (type-tag snd))
                                     (raise/unify (cons snd rest))))
              ((type= fst snd) (cons fst (raise/unify (cons snd rest))))
              (else (error "RAISE/UNIFY" "type error"))))))
(define (apply-generic-numeric op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err)
      (error "No method for these types"
             (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((converted-args (raise/unify args)))
          (if converted-args
              (apply op converted-args)
              (err))))))

;; [ex 2.85] design a proc "drop" that lowers a data object as much as possible. To do so we must
;;           be able to decide whether an object can be lowered. Define a generic proc "project"
;;           op that "pushes" an object down the tower (eg. you would project a complex no. by
;;           taking the real part). Then an object can be lowered if (raise (project obj)) = obj.
;;           design the projection procs, install "project" as a generic proc, use a generic
;;           equality predicate as described in ex 2.79, write "drop" proc to drop an object as
;;           far as possible; use "drop" to rewrite "apply-generic" from 2.84 so it simplifies
;;           its answers.

;; Individual project methods: apply-generic will dispatch to these, passing the contents of
;; the tagged argument only.
(define (project-complex z) (make-real (real-part z)))
(define (project-real x)
  (let iter ((multiplier 1))
    (if (integer? (* x multiplier))
        (simplify-rational
         (make-rational (inexact->exact (* x multiplier))
                        multiplier))
        (iter (* 10 multiplier)))))
(define (simplify-rational q) q)    ; stub - replace with simplification proc
(define (project-rational q)
  ; this proc breaks the rational type's encapsulation, since its interface doesn't provide a
  ; simple way to perform type conversion.
  (make-integer (truncate (/ (car q) (cdr q)))))
(define (project-integer n) #f)
(put 'project '(complex) project-complex)
(put 'project '(real) project-real)
(put 'project '(rational) project-rational)
(put 'project '(integer) project-integer)
(define (project x) (and x    ; if x is #f, return #f
                         (apply-generic 'project x)))

(define (drop x)
  (and x
       (if (equ? (raise (project x))
                 x)
           (drop (project x))
           x)))

(define (apply-generic-simplify op . args)
  (drop (apply apply-generic-numeric (cons op args))))
  

;; Example: Polynomial Arithmetic.
;; -------------------------------
;; We define a polynomial as a variable and a set of terms.
;; A variable is just a symbol, so we can use these defns from sect.2.3.2.

;; Assume we have available a constructor make-poly, and selectors variable and term-list...

;; We'll embed our poly code in an installation package..
(define (install-polynomial-package)
  ;; internal procs
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))
  ;; representation of terms and term-lists
  ;;...
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))  (adjoin-term t1
                                                            (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))  (adjoin-term t2
                                                            (add-terms L1 (rest-terms L2))))
                   (else  (adjoin-term (make-term (order t1)
                                                  (add (coeff t1) (coeff t2)))
                                       (add-terms (rest-terms L1)
                                                  (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; Implementation of terms and term-lists
  ;; our implementation of adjoin-term just conses the term onto the list - hence, we must
  ;;   only use it when the term is higher-order than the highest order existing term, or it
  ;;   won't preserve the order of the term list.
  (define (adjoin-term term term-list) ; adjoin a higher-order term to term list
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (remove-zero-terms term-list)
    (filter (lambda (term) (not (=zero? (coeff term))))
            term-list))
  (define (zero-poly? p) (null? (remove-zero-terms (term-list p))))
  (define (negate-poly p)
    (make-poly (variable p)
               (map (lambda (term)
                      (make-term (order term) (neg (coeff term))))
                    (term-list p))))
  ;; interface to the rest of the system...
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (zero-poly? p)))
  (put 'neg '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (negate-poly p2)))))
  'done)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; [ex 2.87] 
;; =zero? for polys: I initially implemented generic =zero? in terms of generic add and sub,
;; rather than installing a new proc. This will work for polys once a sub method is installed.
;; However, it's more efficient to install a separate procedure in the generic db.
(define (=zero? x) (apply-generic '=zero? x))
;; add the op to scheme-number, rational, complex and polynomial packages.

;; [ex 2.88] add subtraction of polys
;;           (hint: first define generic negation)
(define (neg x) (apply-generic 'neg x))

;; [ex 2.89] Define procs to implement term-list representation appropriate for dense polys
(define (install-dense-poly-package)
  ;; internal
  (define (make-poly variable coeff-list)
    (cons variable coeff-list))
  (define (variable p) (car p))
  (define (coeff-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y)
    (and (variable? x)
         (variable? y)
         (eq? x y)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (map add (coeff-list p1) (coeff-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  ;; interface
  (define (tag p) (attach-tag 'dense-polynomial p))
  (put 'make 'dense-polynomial
       (lambda (var coeffs) (tag (make-poly var coeffs))))
  (put 'add '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
)
(define (make-dense-polynomial var coeffs)
  ((get 'make 'dense-polynomial) var coeffs))

;; ======================
;; TESTING SESSION SETUP.
;; ======================
 
(define (testsession)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-polynomial-package)
  (put 'pow '(scheme-number scheme-number) (lambda (x y) (expt x y)))
  (put 'minimag '(complex complex) (lambda (x y) (min (magnitude x) (magnitude y))))
  (put 'minimag '(sceme-number scheme-number) (lambda (x y) (min x y)))
  'done)
(define mkcomplex make-complex-from-real-imag)
;; start testsession..
(testsession)
(define c (mkcomplex 3 1))
(define d (mkcomplex 4 2))
(define x 7)
(define r (make-rational 3 5))
(define ll (list c x d))
(define sum-ll (foldl add 0 ll))
(define l (list c x d r))
(define n (make-integer 8))
(define p (make-polynomial 'x '((2 1) (0 -5))))
(define q (make-polynomial 'x '((2 3) (1 4))))
;; note: can't add complex to rational yet, since there's no rational->complex proc in the
;;   coercion-procs db. However we can do this indirectly by converting rational->scheme-number
;;   and then scheme-number->complex...
(define addcr (add c (add r 0)))

