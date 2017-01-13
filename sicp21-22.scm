(load "c:/code/scheme/sicp1.3.scm")

;; open a graphics device ('canvas') and implement 'draw-line'
;(define canvas (make-graphics-device 'win32 500 500 'grayscale-128))
(define (draw-line P1 P2)  ;  P1,P2 are points, ie. position vectors
  (graphics-draw-line canvas (vect-xord P1) (vect-yord P1) (vect-xord P2) (vect-yord P2)))
;; note: the origin is in the centre, and co-ordinate values are in the range [-1,1]


;; Ch 2: BUILDING ABSTRACTIONS WITH DATA

;; In ch1, we built abstractions by combining procs to form "compound procedures".
;; Now, we will build abstractions by combining data objects to form "compound data".
;; Compound data objects allow us to treat collections of data as a single conceptual unit.
;; They also allows us to seperate the parts of the program that deal with how data objects are
;; represented from the parts that deal with how data objects are used. This is "data abstraction".
;; Data abstractions allows us to deal with complexity by erecting "abstraction barriers"
;; between different parts of a program.
;; To form compound data objects, a language must provide some sort of 'glue' to combine data objects.
;; There are many possible types of glue. We will learn how to form compound data using only
;; procedures, without any sort of special data operations. This will further blur the line between
;; 'procedure' and 'data'.
;; A key idea regarding compound data is the notion of "closure" - We should be able to ;glue'
;; together not only primitive data, but comound data objects as well.
;; Another key idea is that compound data objects can form "conventional interfaces" for
;; combining program modules.
;; To work with data which may be represented differently in different parts of the program,
;; we will implement "generic operations" which can handle different kinds of data.
;; Maintaining modularity in the presence of generic operations requires more powerful abstraction
;; barriers than those created with data abstraction alone. We will intoduce "data-directed programming"
;; as a technique allowing individual data representations to be design alone and then combined additively.

;; Sec 2.1: INTRODUCTION TO DATA ABSTRACTION.
;; The basic process of data abstraction is to structure the programs using compound data objects
;; so that they operate on 'abstract data', ie. they should make no more assumptions than necessary
;; about the data. At the same time, a concrete representation (implementation) of the compound data
;; object is defined independently of the programs using it. The interface between these two parts of
;; the system is a set of procedures, called "selectors" and "constructors", that implement the abstract
;; data in terms of the concrete representation.

;; rational numbers - we wish to perform arithmetic on rational numbers. We begin by pretending we have
;; procedures make-rat, numer and denom to synthesise a rational from integers and to select its
;; numerator or denominator. This 'wishful thinking' technique is very useful. Later, we will go back
;; and implement these procedures.
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
;; Now, to make our constructor and selectors.
;; Scheme has a compound data structure called a "pair", with constructor cons,
;; and selectors car and cdr to return the left and right element of a pair, respectively.
;; Since our rationals are pairs of integers, we can implement their constructor and selectors
;; directly with cons,car and cdr; or, if we wish, actually use cons,car,cdr as those procs.
(define (make-rat n d) (cons n d)) ; could use:   (define make-rat cons)
(define (numer x) (car x))         ; could use:   (define numer car)
(define (denom x) (cdr x))         ; could use:   (define denom cdr)
;; identifying our constructor/selectors with those for pairs would be more efficient, since each
;; application would require only the one call to the primitive procedure, whereas this way there
;; is an additional call each time. However, this way allows us to use debugging aids that, for example,
;; trace calls to make-rat. If we identified make-rat with cons, such a tool would trace every call to cons.
;; Note pairs can contain any data, including other pairs. Data made from pairs is called "list-structured" data.

;; If there were no primitive procs to make pairs, I could construct them by identifying them with fns.
;; eg (define (pair x y) (lambda (i) (cond ((= i 1) x) ((= i 2) y) (else None))))
;; I could define the null value None as, perhaps, (define None (lambda (x) ()))
;; Thus compound data structures could be make using procs, since they are first-class objects.
;; If I always used pair-specific procs to access and create them, there would be no danger of
;; confusing them with, say, a 2-item sequence fn that happens to have the same form.
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-quarter (make-rat 1 4))
;; revise make-rat to express the fraction in lowest terms using gcd proc
(define (make-rat n d)
  (let ((gr-div (gcd n d)))
    (cons (/ n gr-div)
          (/ d gr-div))))
;; [ex 2.1] Revise make-rat to handle sign correctly, ie. denom always positive
(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((gr-div (gcd n d)))
        (cons (/ n gr-div)
              (/ d gr-div)))))
;; The underlying idea of data abstraction is to identify for each type of data object,
;; a set of operations in terms of which all manipulations of the data objects are expressed.
;; Procs used to implement those operations should never be employed by higher-level routines.
;; This is the concept of an "abstraction barrier".
;; Manipulation of our rationals is done with add-rat, sub-rat, mul-rat, div-rat and equals-rat?.
;; These are implemented with constructor/selectors make-rat, numer and denom, which deal with
;; rationals as numerator-denominator pairs. Adding rationals at the higher level is never done
;; directly using selectors and constructors - it is always done with add-rat. There is an
;; abstraction barrier between these levels. Again, there is an abstraction barrier between the
;; level of numerators and denominators and the level of pairs, in terms of which it is implemented.
;; Rational procedures are defined with make-rat, numer and denom - not directly with cons, car and cdr.
;; There is another abstraction barrier between the primitive pair operators cons, etc., and their
;; implementation details (whatever those details may be).
;; Thus, abstraction barriers seperate different levels of the system. At each level, the barrier
;; seperates the programs (above) that use an abstraction from the programs (below) that implement
;; that abstraction.
;; The procedures at each level are the interfaces that form the abstraction barrier and connect
;; the different levels.
;; ** Such a set of procedures forms a single bridge between levels - all concourse is channeled
;; ** through them. This makes a complex program easier to manage.

;; [ex 2.2] Representation of points and line segments

(define (make-point x y) (cons x y))  ; point constructor
(define (x-point p) (car p))          ; point selector (x-value)
(define (y-point p) (cdr p))          ; point selector (y-value)

(define (make-segment p1 p2) (cons p1 p2))  ; interval constructor
(define (start-segment s) (car s))          ; interval selector (start-point)
(define (end-segment s) (cdr s))            ; interval selector (end-point)

(define (midpoint p1 p2)
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))
(define (midpoint-segment s)
  (midpoint (start-segment s) (end-segment s)))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; [ex 2.3] Design 2 representation for rectangles in the plane.
;;          Write procs for area and perimeter which are independent of representation.

;; For a minimal set of numbers specifying a unique rectangle, there will be 5 numbers.

;; (1) A rectangle could be defined by 2 pts p1,p2 defining a diagonal and an angle theta
;; representing the direction of a side adjacent to p1 (where theta is measured from the
;; diagonal specified, anticlockwise about p1).

;; (2) A rectangle could be represented by 'building it' from the origin along the axes,
;; with a=length along x-axis, b=length along y-axis, then translating the vertex at O to
;; point (x,y) and rotating counterclockwise by angle theta. (If we imagine starting with
;; a unit square instead, the side lengths become stretching factors, and each of the 5
;; parameters represents an angle-preserving transformation).

;; It's not necessary for the exercise, but ideally, we would want a test for identity of two
;; rectangles, and preferably, a 'canonical' representation of a given rectangle.
;; We could do this using method 2 and additional restrictions, eg. the longer side should
;; lie along x-axis and the angle must be -pi/2 < theta <= pi/2.
;; Alternatively, if we adopt a representation with redundancy, we can specify the four vertices
;; using some ordering relation to ensure uniqueness. This is probably the simplest approach
;; conceptually.

;; some basic tools: polar constructor/selectors for points, vector +/- & O-relection of pts.
(define (make-point-polar r theta)
  (make-point (* x (cos theta))
              (* y (sin theta))))
(define (r-point p)
  (sqrt (+ (square (x-point p)) (square (y-point p)))))
(define (theta-point p)
  (atan (/ (y-point p)
           (x-point p))))
(define (vector-add-points p1 p2)
  (make-point (+ (x-point p1) (x-point p2))
              (+ (y-point p1) (y-point p2))))
(define (reflect-point p)
  (make-point (- (x-point p))
              (- (y-point p))))
(define (vector-sub-points p1 p2)
  (vector-add-points p1 (reflect-point p2)))

(define (angle-incline-points p1 p2)
  (theta-point (vector-sub-points p1 p2)))
(define (angle-incline-segment s)
  (angle-incline-points (start-segment s) (end-segment s)))
(define (distance-between-points p1 p2)
  (r-point (vector-sub-points p1 p2)))
(define (length-segment s)
  (distance-between-points (start-segment s) (end-segment s)))
(define (make-point-at-incline p1 d theta)
  (vector-add-points p1 (make-point-polar d theta)))
(define (make-inclined segment start-pt length theta)
  (make-segment start-pt
                (make-point-at-incline start-pt length theta)))

;; for now, I'll represent a rectangle by a pair of coterminal segments
;; make-rectangle p1 p2 p3 makes rectangle defined by segments (p1,p2) and (p1,p3)
(define (make-rectangle p1 p2 p3)       ; constructor for rects
  (cons (make-segment p1 p2)
        (make-segment p1 p3)))
(define (side1-rectangle r) (car r))    ; selector for first side of rect
(define (side2-rectangle r) (cdr r))    ; selector for second side of rect
;; area and perimeter
(define (area-rectangle r)
  (* (length-segment (side1-rectangle r))
     (length-segment (side2-rectangle r))))
(define (perimeter-rectangle r)
  (* 2 (+ (length-segment (side1-rectangle r))
          (length-segment (side2-rectangle r)))))
;; TODO: Create other repesentations of ectangles


;; [ex 2.4] An alternative procedural representation of pairs.
(define (newcons xy)
  (lambda (m) (m x y)))
(define (newcar z)
  (z (lambda (p q) p)))
(define (newcdr z)
  (z (lambda (p q) q)))
;; [ex 2.5] We can represent pairs of nonnegative integers using only
;; numbers and arithmetic if we represent (a,b) as 2^a*3^b
;; ** note: This should work fine for pairs with negative ints as well,
;; ** but then we'd have to deal with rationals rather than just natural numbers.
(define (pr-cons a b)
  (* (expt 2 a) (expt 3 b)))
(define (pr-car p)
  (order-of-factor 2 p)) ; using 'wishful thinking', use proc 'order-of-factor' not yet written
(define (pr-cdr p)
  (order-of-factor 3 p))
(define (order-of-factor p n) ; computes how many times the factor p (usually prime) divides n
  (define (iter p n count)
    (if (divides? p n)
        (iter p
              (/ n p)
              (+ count 1))
        count))
    (iter p n 0))

;; [2x 2.6] In a language that can manipulate procedures, we can Iif we really want) get by
;; without numbers at all. The Church Numerals represent natural numbers as procedures:
(define ch-zero (lambda f (lambda (x) x)))
(define (ch-add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;; define 1 and 2 directly:
(define ch-one (lambda (f) (lambda (x) (f x))))
(define ch-two (lambda (f) (lambda (x) (f (f x)))))
;; zero takes any fn f, returns identity
;; one takes any fn f, returns f
;; two takes any fn f, returns f.f (ie.composition),...etc

;;--------------------------------------------------------------------------------------------------
;; sec 2.1.4. Extended Exercise
;; INTERVAL ARITHMETIC
;; -------------------
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y) ; not very efficient
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (reciprocate-interval x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))
(define (div-interval x y)
  (mul-interval x (reciprocate-interval y)))
;; [ex 2.7] implementation of interval constructor & selectors
(define (make-interval x y) (cons x y))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; [ex 2.8] sub-interval
(define (negate-interval x)
  (make-interval (- (upper-bound x))
                 (- (lower-bound x))))
(define (sub-interval x y)
  (add-interval x (negate-interval y)))
;; [ex 2.9]
;; w(x)=u(x)-l(x)
;; w(x+y)=u(x+y)-l(x+y)=u(x)+u(y)-{l(x)+l(y)}={u(x)-l(x)}+{u(y)-l(y)}
;; similarly for subtraction
;; multiplication: consider x=(10,11),y=(1,2) both have width 1
;; x*x=(100,121) - width 21
;; y*y=(1,4) - width 3
;; x/x=(10/11,11/10)
;; y/y=(1/2,2)

;; [ex 2.10] division-by-zero-spanning-interval
(define (within-interval a x) ; a is a number, x is an interval
  (and (> a (lower-bound x))
       (< a (upper-bound x))))
;; I used a proc reciprocate-interval to build div-interval,
;; so I only need to add checking to this.
(define (reciprocate-interval x)
  (if (within-interval 0 x)
      error
      (make-interval (/ 1.0 (upper-bound x))
                     (/ 1.0 (lower-bound x)))))
;; [ex 2.11] mul-interval optimization
(define (pos-interval? x) (positive? (lower-bound x)))
(define (neg-interval? x) (negative? (upper-bound x)))
(define (zero-interval? x) (within-interval 0 x))
(define (mul-interval x y)
  (cond ((and (pos-interval? x) (pos-interval? y))
         (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (pos-interval? x) (zero-interval? y))
         (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (pos-interval? x) (neg-interval? y))
         (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
        ((and (zero-interval? x) (pos-interval? y))
         (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (zero-interval? x) (zero-interval y))
         (make-interval (min (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y))
                             (* (upper-bound x) (upper-bound y)))))
        ((and (zero-interval? x) (neg-interval? y))
         (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (neg-interval? x) (pos-interval? y))
         (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
        ((and (neg-interval? x) (zero-interval? y))
         (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
       ((and (neg-interval? x) (neg-interval? y))
         (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))))

;; note: the 'width' here is the distance from the centre of the interval to an endpoint.
;; ie. it is half the width of an interval, as defined earlier in th section.
(define (make-centre-width c w)
  (make-interval (- c w) (+ c w)))
(define (centre i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;; [ex 2.12] centre/percent construcor/selectors
(define (% x) (/ x 100.0))
(define (make-centre-percent c p)
  (make-interval (* c (% (- 100 p))) (* c (% (+ 100 p)))))
(define (percent i)
  (* 100 (/ (- (upper-bound i) (lower-bound i))
            (+ (upper-bound i) (lower-bound i)))))

;; [ex 2.12] Approximate formula for percentage tolerance of product of 2 positive intervals
;; consiser the tolerances as fractions rather than percentages, to simplify calculation.
;; i1 = ( c1(1-p1) , c1(1+p1) ) and i2 = ( c2(1-p2) , c2(1+p2) )
;; then i1*i2 will be ( c1c2(1-(p1+p2)+p1p2) , c1c2(1+(p1p2)+p1p2) )
;; we can see that in general, the centre of the product is not the product of the centres.
;; However, for small percentage tolerances, p1p2 is tiny, so the centre of the product *is*
;; (approximately) the product of the centres, and the tolerance of the product is approximately
;; the sum of the tolerances. p(i1*i2)=p1+p2 approx.

;; [ex 2.13] tolerances not conserved under algebraic rearrangement
(define (par1 r1 r2) ; parallel resistors
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (reciprocate-interval (add-interval (reciprocate-interval r1)
                                      (reciprocate-interval r2))))
;; obviously, the tolerance is not conserved under rearrangement. After all, a non-sero interval x
;; will not give (1.0,1.0) when divided by itself. The system does not 'know' the identical intervals
;; actually represent the same quantity.
;; Tests:
(define (print-centre-percent i)
  (display (centre i))
  (display "    ;    ")
  (display (percent i)))
;; Each operation (with small percentage tolerance) adds the tolerances of the operands.
;; So if a has tolerance 0.5%, a*a/a, or 2a-a, has about 1.5%
;; [ex 2.15] par2 is better than par 1
(define (compare-par-progs r1 r2)
  (display "par1:   ")
  (print-centre-percent (par1 r1 r2))
  (newline)
  (display "par2:   ")
  (print-centre-percent (par2 r1 r2)))
;; par1 has 3 times the percentage tolerance as par2, as expected, since par2 only combines
;; 2 intervals once, whereas par2 does so 3 times.
;; [ex 2.16] As explained above, when a quantity is combined more than once in an algebraic
;; expression, the tolerance is increased each time. The system doesn't know the multiple
;; instances are really the same quantity - they could be different quantities with the same
;; range of values, in which case the behaviour of the system is correct.
;; An expression should be converted to its simplest form (least combinations of intervals)
;; to avoid spuriously magnifying the tolerance.
;; To build an interval-arithmetic package without this shortcoming would be diificult, but
;; not impossible. It would probably require some sort of symbolic algebra system, or a way
;; of parsing an expression and maintaining information about quantities encountered until the
;; entire expression has been read.
;; One definite requirement is that intervals would require an 'identity' - that is, the bounds
;; would not be enough; the system must know if two (99,101) intervals represent the same quantity,
;; or independent ones.
;; TODO: Attempt this. Start with just + and -, then * and /, then all four.

;; Sec 2.2: HEIRARCHICAL DATA AND THE CLOSURE PROPERTY.
;; A standard way to visualise a pair is the "box and pointer notation", where a pair is 
;; represented by two adjacent boxes. The first contains a pointer to the car, the second a pointer
;; to the cdr.
;; As we've seen, we can create pairs whose members are themselves pairs. This is called the
;; "closure property" and is the key to any powerful means of combination, as it allows the
;; construction of heirarchical structures.
;; We've already made use of the closure property of procedures.
;; We will now make use of the closure property in compound data, including implementation
;; of conventional techniques for using pairs to represent trees and sequences.

;; Sec 2.2.1: Representing Sequences.
;; There are many ways to represent sequences with pairs, but a particularly simple one is called
;; a "list". A list represents a sequence as a chain of pairs. The car of each pair is the
;; corresponding element of the sequence; the cdr of each pair holds the next pair. The cdr of the
;; final pair holds a special value whch is not a pair, indicating the end of the list. For now,
;; we'll use the value of the variable nil (an empty list).
;; The entire sequence is built out of nested cons operations, eg:
;; (cons 1 (cons 2 (cons 3 (cons 4 nil)))) is the list representing {1,2,3,4}
;; Scheme provides a primitive for building lists: (list 1 2 3 4) is the same as the above.
;; (car l) returns the 1st item of l; (cdr l) returns the sublist starting from the 2nd item.
;; Nested applications of car and cdr can extract subsequent elements, eg. (car (cdr (cdr l)))
;; returns the 3rd item in l. (cons e l) adds e to the start of l - (cons 5 (list 1 2 3)) is (5 1 2 3)
(define nil (list))

;; indexing of lists conventionally begins at 0
(define (list-ref ls n) ; returns element n of list ls (starts at 0)
  (if (= n 0)
      (car ls)
      (list-ref (cdr ls) (- n 1))))
(define (length ls)
  (if (null? ls)
      0
      (+ 1 (length (cdr ls)))))
;; iterative version of length
(define (length ls)
  (define (iter ls count)
    (if (null? ls)
        count
        (iter (cdr ls) (+ count 1))))
  (iter ls 0))
;; A common technique in list programming is to "cons up" an 'answer list' while you "cdr down" the
;; original list. This is illustrated in append:
(define (append ls1 ls2)
  (if (null? ls1)
      ls2
      (cons (car ls1)
            (append (cdr ls1) ls2))))
;; [ex 2.17] last-pair
(define (last-pair ls)
  (if (= 1 (length ls))
      ls
      (last-pair (cdr ls))))
; [ex 2.18] reverse
(define (reverse ls)
  (if (= 1 (length ls))
      ls
      (append (reverse (cdr ls)) (list (car ls)))))
;; [ex 2.19] count-change procedure with denomination-list argument
(define (cc amount coin-values) ; calculates no. ways to make amount with coins in list coin-values
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((no-more? coin-values) 0)
        (else (+ (cc (- amount (first-denomination coin-values))
                     coin-values)
                 (cc amount
                     (except-first-denomination coin-values))))))
(define (first-denomination c) (car c))
(define (except-first-denomination c) (cdr c))
(define (no-more? c) (null? c))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define au-sil-coins (list 50 20 10 5))
(define au-old-coins (list 50 20 10 5 2 1))
(define au-coins (list 200 100 50 20 10 5))
;; The order of coin-values does not matter. The algorithms just counts by splitting the
;; possibilities at each point into those where denom x is used and those where it isn't
;; (of course, once denom x is not used, it can't be used later - the coin sums are ordered
;; in the same way as the list.
;; The 'boundary cases' are not dependent on the order of coin-values. This is an advantage
;; of this code (testing for amount<0) over the method I would have used (testing for
;; amount<first-denom). If I'd done it that way it would have been order-dependent and I would
;; have had to sort the list to remove the dependence.

;; "Dotted tail notation": In a procedure definition, you can add an extra parameter at the end,
;; preceded by a dot - when the proc is called, any 'excess' args are passed to this param as a list.
;; note on lambda defs: (define (f x y . z) <body>) is (define f (lambda (x y . z) <body>))
;;                      (define (g . z) <body>) is just (define g z <body>)
;; [ex 2.20] same-parity
(define (same-parity n . ls)
  (define (iter n ls result)
  (if (null? ls)
      result
      (let ((first (car ls)))
        (if (eq? (even? n) (even? first))
            (iter n (cdr ls) (cons first result))
            (iter n (cdr ls) result)))))
  (iter n ls (list)))
;; define parity? and make recursive version retaining order in filtered list
(define (parity n)
  (if (odd? n)
      1
      0))
;; I don't yet know how to 'unpack' a list to pass it into a proc as an arbitrary
;; number of arguments. Therefore, I use the nested recurs proc, which takes
;; an explicit list argument.
(define (same-parity n . ls)
  (define (recurs n ls)
    (if (null? ls)
        (list)
        (let ((first (car ls)))
          (if (= (parity n) (parity first))
              (cons first (recurs n (cdr ls)))
              (recurs n (cdr ls))))))
  (recurs n ls))
;; note: this procedure could be easily modified to make a general 'filter' proc.

;; reverse-list
(define (reverse-list ls)
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls) (cons (car ls) result))))
  (iter ls (list)))

;; Mapping over lists.
(define (scale-list items factor) ; scales each element of items by factor
  (if (null? items)
      nil
      (cons (* factor (car items))
            (scale-list (cdr items) factor))))
;; we can abstract this pattern as a higher-order proc, map.
;; map takes as its arguments a proc of 1 arg and a list, and returns a new list
;; formed by applying the proc to each element of the input list
(define (newmap proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (newmap proc (cdr items)))))
;; redefine scale-list in terms of map
(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
;; the importance of map is not just as a tool for applying a common pattern, but as
;; an abstraction barrier, hiding the element-by-element processing details of the list
;; manipulation and allowing us to think in terms of transformations.
;; Like other abstraction barriers, this gives us the freedom to cahnge low-level
;; implementation details while preserving the conceptual framework.
;; [ex 2.21] square-list
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list items)
  (map square items))
;; [ex 2.22] Iterative square-list
;; the example cons the square of the first item in each step with the previous result.
;; Thus, as it works through the list, each square is placed before the square of the preceding item.
;; The 2nd example will produce a pair with nesting at the wrong end, ie. a 'reversed list', since
;; it pairs each square to the right of the previous result.
;; You need to use a 2-stage process for an iterative version.

;; [ex 2.23] for-each
;; for-each is similar to map, but rather than returning a list of values, for-each simply
;; applies proc to each element in items, ignoring return values. ie. proc is called on each
;; value solely for its side-effects.
(define (new-for-each proc items)
  (define (fe-apply proc items)
    (proc (car items))
    (new-for-each proc (cdr items)))
  (if (null? items)
      #t  ; Return value is arbitrary: I'm using #t (as per SICP suggestion) since I don't know how to express void.
      (fe-apply proc items)))

;; I still don't know if I can include a block of expressions in an if
;; Also, I don't think you can force a 'return' from a procedure. These two factors
;; make it difficult to write procs where I need to test a condition and then, if it fails,
;; call more than one proc. I keep having to define helper procs.

;; Sec 2.2.2. Heirarchical Structures.
;; The representation of sequences in terms of lists generealizes naturally to include
;; sequences whose elements may themselves be sequences. Eg, (cons (list 1 2) (list 3 4))
;; constructs ((1 2) 3 4) which may be regarded as a list of 3 items, the first of which
;; is a list of 2 items. Another way to look at such a sequence is as a tree. This sequence
;; represents a tree with 3 branches: the first splits into 2 branches, ending in 1 and 2;
;; the second and third branches are straight, ending in 3 and 4 respectively.
;; Recursion is a natural tool for dealing with trees, since an operation on a tree can often
;; be reduced to an operation on its branches, which can then be recursively reduced further.

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1) ; note: this needs to come after the null? test, since the
        ;                     empty list also satisfies this, but must not be counted.
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;; this works on 'improper lists' as well as proper lists.

;; [ex 2.24] (list 1 (list 2 (list 3 4)))
;; represented as a list by (1 (2 (3 4))) - it can be represented by a tree with 2 top-level
;; branches. The first ends in 1, the second branches in two. The first of these branches ends
;; in 2, while the second splits into two braches, ending in 3 and 4.
;; Represented as a box-and-pointer structure depicting pairs:
;; <A> at 'top level', there are 2 pairs of boxes (representing a 2-element list).
;; The first pair has a pointer to 1 in the left box and a pointer to the second pair in the right box.
;; The second pair has a pointer to the sublist (2 (3 4)) in the left box and nil in the right box.
;; <B> sublist (2 (3 4)) is represented by 2 pairs of boxes.
;; [ ptr to 2 | ptr to--]--->[ ptr to (3 4) | nil ]
;; <c> sublist (3 4) is represented by 2 pairs of boxes.
;; [ ptr to 3 | ptr to--]--->[ ptr to 4 | nil ]
;; -------------experiment with ASCII box-ptr notation---------------------

;      [ 1 | * ]-->[ * | X ]
;                    |
;                    V
;                  [ 2 | * ]-->[ * | X ]
;                                |
;                              [ 3 | * ]-->[ 4 | X ]

;;---------------------------------------------------------------------------
;; seems OK.

;; [ex 2.26] (define x (list 1 2 3))   (define y (list 4 5 6))
;; (append x y) returns (1 2 3 4 5 6)
;; (cons x y) returns ((1 2 3) 4 5 6)
;; (list x y) returns ((1 2 3) (4 5 6))

;; [ex 2.27] deep-reverse
(define (deep-reverse x)
  (define (iter x result)
    (cond ((null? x) result)
          ((not (pair? (car x))) (iter (cdr x)
                                       (cons (car x) result)))
          (else (iter (cdr x)
                      (cons (iter (car x) (list)) result)))))
  (iter x (list)))

(define (fringe x)
  (cond ((null? x) nil)
        ((null? (car x)) (fringe (cdr x)))
        ((not (pair? (car x))) (cons (car x) (fringe (cdr x))))
        (else (append (fringe (car x)) (fringe (cdr x))))))
;; clumsy and over-complicated. Re-implement:
(define (fringe x)
  (cond ((null? x) nil)
        ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
        (else (list x))))

;; [ex 2.28] Binary Mobile.
(define (make-mobile left right) (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (make-branch length structure) (list length structure))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define (total-weight m)
 (+ (branch-weight (left-branch m))
    (branch-weight (right-branch m))))
(define (branch-weight b)
  (if (not (pair? (branch-structure b)))
      (branch-structure b)
      (total-weight (branch-structure b))))
(define (branch-torque b)
  (* (branch-weight b)
     (branch-length b)))
(define (branch-mob-balanced? b) ; tests if the submodule attached to b, if any, is balanced
  (if (not (pair? (cadr b)))
      #t
      (mobile-balanced? (cadr b))))
(define (mobile-balanced? m)
  (and (= (branch-torque (left-branch m))
          (branch-torque (right-branch m)))
       (branch-mob-balanced? (left-branch m))
       (branch-mob-balanced? (right-branch m))))
;; changing constructor/selector implementation
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
;; The second selector for each needs to call cdr rather than cadr
(define (right-branch m) (cdr m))
(define (branch-structure b) (cdr b))
;; so does branch-mob-balanced.
;; better yet, use branch-structure, as I should have in the first place.
(define (branch-mob-balanced? b)
  (if (not (pair? (branch-structure b)))
      #t
      (mobile-balanced? (branch-structure b))))
;; note: if we changed to an implementation that didn't use pairs, we'd have to
;; change all the pair? tests. We should have implemented mobile? and branch? procs.

;; Mapping over trees.
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
;; redefine scale-tree using map
(define (scale-tree tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* subtree factor)))
       tree))
;; [ex 2.30] square-tree
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))
;; ...and using map..
(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (square subtree)))
       tree))

;; [ex 2.31] tree-map
(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map proc subtree)
             (proc subtree)))
       tree))

;; [ex 2.32] subsets
(define (subsets s) ; a set s is represented as a list of distinct items
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))
;; the proc generates subsets recursively by appending the set of subsets
;; not containing the first element with the set of subsets containing the
;; first element. The latter set is constructed from the former by consing
;; the first element to every element set of the 'not-containing' set with map

;; Sec 2.2.3. Sequences as Conventional Interfaces.
;; Working wiht compound data, we've seen how abstraction allows us to work
;; without worrying about, or being dependent on, specific data representations.
;; Another powerful design principle for working with data structures is the use
;; of "conventional interfaces".
(define (sum-odd-squares tree) ; sums the squares of leaves with odd values
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree)
                                (square tree)
                                0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (even-fibs n) ; returns a list of even fib numbers with index<=n
  (define (next k)
    (if (> k n) ; keeps recursing until k>n, then adds nil and returns
        nil     ; this final addition of nil makes a proper list
        (let ((f (fibonacci k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
;; although these two programs look quite different, there is a great deal of
;; similarity at a more abstract level.
;; sum-odd-squares : * enumerates the leaves of a tree
;;                   * filters them, selecting odd ones
;;                   * computes the squares of these
;;                   * accumulates the results using +, starting at 0
;; even-fibs : * enumerates the integers from 0 to n
;;             * computes the Fibonacci number for each
;;             * filters them, selecting the even ones
;;             * accumulates the results using cons, starting with nil
;; A signal-processing engineer would find it natural to conceptualize these
;; processes in terms of signals flowing through a cascade of stages, each
;; of which implements part of the program plan.
;; enumerator, filter, map (a type of transducer), accumulator.
;; Unfortunately, the proc defs above fail to exhibit this signal-flow structure.
;; The procs do not contain distinct parts corresponding to the signal-flow elements.
;; We will increase the conceptual clarity of our programs by rewriting these procs
;; to make the signal-flow structure manifest.

;; Sequence Operations.
;; if we wish our procs to reflect the signal-flow structure, the key is to look at
;; the "signals" that travel from one stage  of the process to the next.
;; If we represent these signals as lists, we can use list operations to implement
;; the processes at each stage (eg. map for the mapping stage).
;; We'll need list procedures for the other stages.
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else  (filter predicate (cdr sequence)))))
(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))
;; All that remains to implement siganl-flow diagrans is to enumerate the sequence of
;; elements to be processed.
;; For even-fibs, we need to generate a list of integers in a given range:
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))
;; To enumerate the leaves of a tree, we can use fringe, but we shall rename it:
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (append (enumerate-tree (car tree))
                              (enumerate-tree (cdr tree))))
        (else (list tree))))
;; Now we can reformulate our procs as signal-flow structures.
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fibonacci
                           (enumerate-interval 0 n)))))
;; Expressing programs as sequence operations allows us to make our designs modular.
;; Modular design can be encoutaged by providing a library of standard functions and
;; a standard interface for connecting the components in flexible ways.
;; Modular Design is a powerful strategy for comtrolling complexity in engineering design.
;; In real signal-processing, designers regularly build systems by cascading elements
;; chosen from standardized families of filters and transducers.
;; In the same way, sequence operations provide a library of standard program elements
;; which can be mixed-and-matched.
(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fibonacci
                        (enumerate-interval 0 n)))))
(define (product-of-odd-element-squares sequence)
  (accumulate *
              1
              (map square
                   (filter odd?
                           sequence))))
;; We can also formulate conventional data processing operations as sequence operations.
;; Suppose we have a data strucure record, storing personnel records. Given a list of records,
;; a selector 'salary' and a predicate 'programmer?', we can construct a procedure to find
;; the salary of the highest-paid programmer:
;(define (salary-of-highest-paid-programmer records)
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer?
;                           records))))
;;
;; Sequences serve as a conventional interface that allows us to combine processing modules.
;; Also, if we uniformly represent structures as sequences, we have localized our data-structure
;; dependencies to a few basic sequence operations. We can experiment with changing the
;; underlying representation of sequences without impacting our overall program design.
;; Later, we will utilize this abstraction barrier to implement infinite sequences.
;; [ex 2.33] Implementation of sequence operations as accumulations.
(define (newmap p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))
(define (length sequence)
  (accumulate (lambda (x y) (inc y))
              0
              sequence))

;; [ex 2.34] Horner's Rule: polynomial evaluation.
;; Horner's Rule is an optimal algorithm for polynomial evaluation:
;; a[n]x^n+a[n-1]x^n-1+...+a[1]x+a[0] is evaluated as (...(a[n]x+a[n-1])x+..a[1])x+a[0]
(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x
                      higher-terms)))
              0
              coeff-seq))

;; [ex 2.35] count-leaves as an accumulation
(define (count-leaves t)
  (accumulate (lambda (leaf-list count) (+ (length leaf-list)
                                           count))
              0
              (map enumerate-tree t)))

;; [ex 2.36] accumulate-n
(define (accumulate-n op init seqs) ; seqs is a sequence of sequences, all of same length
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; [ex 2.37] vector algebra
;; represent vectors by sequences of numbers, matrices as sequences of vectors (the rows of the matrices)
(define (dot-product v w)
  (accumulate +
              0
              (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))
(define (transpose m)
  (accumulate-n cons
                nil
                m))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols))
         m)))

;; [ex 2.38] fold-left
;; accumulate is also known as fold-right, as it works from the right, combining each
;; element with the previous accumulated result. There is also a fold-left:
(define fold-right accumulate)
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))
;; (fold-right / 1 (list 1 2 3)) is 3/2, with fold-left: 1/6
;; (fold-right list nil (list 1 2 3)) is (1 (2 (3 ()))) ; with fold-left: (((() 1) 2) 3)
;; If op is associative, fold-left and fold-right will produce the same values for any sequence.

;; [ex 2.39] reverse in terms of folds.
(define (reverse seq)
  (fold-right (lambda (x y) (fold-right cons (list x) y))
              nil
              seq))
(define (reverse seq)
  (fold-left (lambda (x y) (fold-left cons y (list x)))
             nil
             seq))

;; Nested Mappings.
;; We can extend the sequence paradigm to include amny cpmutations normally expressed
;; as nested loops. For example, consider this problem:
;; for a +ve int n, find all pairs of ints (i,j) s.t 1<=i<j<=n, with i+j is prime.
;; note: the case with i<=j is equivalent, sice i+j is never prime is i=j
;; A natural way to organize the computation is to generate the ordered pairs, filter
;; for primality of sum, then produce a triple (i,j,i+j) for succesful pairs.
;; To generate the sequence of pairs, we can do the following (note: we are using 2-element lists
;; for pairs, *not* Lisp pairs):
;; * enumerate integers from 1 to n
;; * map along this sequence, for each i, enumerate integers from 1 to i-1
;; * map along this sequence, generating pairs (i,j) - we end up with a sequence of (i,j)
;;   pairs for each i.
;; * accumulate this sequence of sequences with append, to combine them into a single sequence of pairs.
(define (generate-ordered-pairs n)
  (accumulate append
              nil
              (map (lambda (i) (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
;; This combination of mapping over a sequence to produce a sequence of sequences, and then accumulating
;; with append is so common, we will isolate it as a seperate procedure.
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;; now, the filter predicate
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
;; now, the procedure to take a prime-sum pair and generate the result triple
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;; combine to make entire procedure:
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i) (map (lambda (j) (list i j))
                                         (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

;; nested mappings are also good for sequences other than interval-enumerations
;; for instance, to generate all permutations of a set S, we could:
;; * for each x in S, recursively generate the permutations of S-{x}
;; * adjoin x to the front of each permutation to make all permutations of S containing x
;; * combine these sequences for all x
(define (permutations s)
  (if (null? s)  ; must be (()) not (), because x will be added to each seq in the seq.
      (list nil) ; also, the empty set has one (empty) permutation, not none.
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))
;; [ex 2.40] unique-pairs
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; [ex 2.41] triple-sum-to
(define (triple-sum-to s n)  ; finds all triples (i,j,k) of distinct integers k<j<i<=n which sum to s
  (filter (lambda (t) (= s (+ (car t) (cadr t) (caddr t))))
          (flatmap (lambda (i) 
                     (map (lambda (pair) (cons i pair))
                          (unique-pairs (- i 1))))
                   (enumerate-interval 1 n))))

;; [ex 2.42] eight-queens puzzle.
;; There are a couple of obvious ways to represent the queens:
;; (a) represent each queen as a pair (row,col), so a given board position is a list of pairs
;;     Another queen is added (to any square) by adding another pair to the list.
;; (b) represent a (n x k) board position as a list of k elements, each between 1 and n, giving
;;     the row in which the queen in that column is placed. A new column and queen would be added
;;     by adding an element to the list. This approach automatically enforces the condition that
;;     there can only be one queen per column.
;; I think (a) is the approach they're thinking of, since it uses lists of pairs.
;; I suspect (b) may be better for this problem, although it couldn't be used for representation of
;; more general positions.
(define (queens board-size)
  (define (queen-cols k)  ;  no. ways to place k queens on a (n x k) board
    (if (= k 0)
        (list empty-board)
        (filter (lambda (position) (safe? k position))
                (flatmap
                 (lambda (sub-position)
                   (map (lambda (row)
                          (adjoin-position row k sub-position))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (define (adjoin-position row numcols sub-position)
    (cons (list row numcols) sub-position))
  (define empty-board nil)
  ;; --- ORIGINAL SAFE? DOESN'T WORK ---
;  (define (safe? numcols position)
;    (if (< (length position) 2)
;        #t
;        (let ((new (car position))
;              (previous (cdr position)))
;          (not (= 0 (accumulate *
;                                1
;                                (map (lambda (q) (* (abs (- (car new) (car q)))
;                                                    (abs (- (cadr new) (cadr q)))
;                                                    (- (abs (- (car new) (car q)))
;                                                       (abs (- (cadr new) (cadr q))))))
;                                     previous)))))))
  ;; --- RE-IMPLEMENTED WITH MODULAR DESIGN ---
  (define (safe? numcols position)
    (define (pair-safe? q1 q2)
      (let ((d-row (abs (- (car q1) (car q2))))
            (d-col (abs (- (cadr q1) (cadr q2)))))
        (not (= 0 (* d-row
                     d-col
                     (- d-row d-col))))))
    (let ((new-queen (car position))
          (sub-position (cdr position)))
      (accumulate proc-and
                  #t
                  (map (lambda (q) (pair-safe? q new-queen))
                       sub-position))))
  (queen-cols board-size))
(define (proc-and bool1 bool2)  ;  a procedural version of "and"
  (and bool1 bool2))
(define (all-true seq)
  (accumulate proc-and #t seq))


;; aside - procedures that I was going to use in queens, but didn't
(define (two-combinations s)  ;  returns a sequence of 2-combinations of s
  (if (null? s)
      nil
      (append (map (lambda (x) (list (car s) x)) (cdr s))
              (two-combinations (cdr s)))))
(define (is-element? item seq)
  (cond ((null? seq) #f)
        ((= (car seq) item) #t)
        (else (is-element? item (cdr seq)))))
(define (values-distinct? seq)
  (if (null? seq)
      #t
      (and (not (is-element? (car seq) (cdr seq)))
           (values-distinct? (cdr seq)))))

;;; another approach to eight-queens
;;; --- DOES NOT WORK!!! ---
;(define (make-queen row col) (list row col))
;(define (queen-col q) (cadr q))
;(define (queen-row q) (car q))
;(define (make-board . queens) queens)
;(define (board-newest b) (car b))
;(define (board-previous b) (cdr b))
;(define (queens-check? q1 q2)
;  (or (= (queen-row q1) (queen-row q2))
;      (= (queen-col q1) (queen-col q2))
;      (= (abs (- (queen-row q1) (queen-row q2)))
;         (abs (- (queen-col q1) (queen-col q2))))))
;(define (board-newest-check? b)
;  (define (iter sub-board)
;    (cond ((null? sub-board) #f)
;          ((queens-check? (board-newest b) (board-newest sub-board)) #t)
;          (else (iter (cdr sub-board)))))
;  (if (not (pair? b))
;      #f
;      (iter (cdr b))))
;(define (board-add q b) (cons q b))
;(define (make-boardlist . boards) boards)
;
;(define (adjoin-position row col old-board)
;  (board-add (make-queen row col) old-board))
;
;(define (queens board-size)
;  (define (queen-cols k)  ;  (n x k) sub-board
;    (if (= k 0)
;        (make-boardlist (make-board nil))
;        (filter board-newest-check?
;                (flatmap (lambda (prev-boardlist)
;                           (map (lambda (newqueen-row)
;                                  (adjoin-position newqueen-row k prev-boardlist))
;                                (enumerate-interval 1 board-size)))
;                         (queen-cols (- k 1))))))
;  (queen-cols board-size))
;;; --- TODO: DEBUG THIS VERSION (IF I CAN BE BOTHERED ---

;; [ex 2.43] interchanged mappings in flatmap.
;; In the altered procedure, the queen-cols sub-problem of size (k-1) is computed
;; seperately for each row value, rather than once, as in the original procedure.
;; So if board-size is n and B[k] is the (n x k) sub-board, it takes n computations
;; of B[n-1] to compute B[n], n computations of B[n-2] to compute B[n-1] once, and
;; so forth. A very crude approximation of the time to complete the altered proc
;; would be (n^n)*T.
;; TODO: Do a more accurate analysis of the time-complexity.
;; TODO: Time the two procedures to test predictions.
;; --- start altered queens ---
(define (alt-queens board-size)
  (define (queen-cols k)  ;  no. ways to place k queens on a (n x k) board
    (if (= k 0)
        (list empty-board)
        (filter (lambda (position) (safe? k position))
                (flatmap
                 (lambda (row)
                   (map (lambda (sub-position)
                          (adjoin-position row k sub-position))
                        (queen-cols (- k 1))))
                 (enumerate-interval 1 board-size)))))
  (define (adjoin-position row numcols sub-position)
    (cons (list row numcols) sub-position))
  (define empty-board nil)
  (define (safe? numcols position)
    (define (pair-safe? q1 q2)
      (let ((d-row (abs (- (car q1) (car q2))))
            (d-col (abs (- (cadr q1) (cadr q2)))))
        (not (= 0 (* d-row
                     d-col
                     (- d-row d-col))))))
    (let ((new-queen (car position))
          (sub-position (cdr position)))
      (accumulate proc-and
                  #t
                  (map (lambda (q) (pair-safe? q new-queen))
                       sub-position))))
  (queen-cols board-size))
;; --- end altered queens ---
;;; timed comparison test
;(define (compare-queens n)
;  (define (iter count results)
;    (if (> count n)
;        results
;        (co-iter count results)))
;  (define (co-iter count results)
;    (iter (+ count 1) (cons (/ (time-proc alt-queens n)
;                               (time-proc queens n))
;                            results)))
;  (iter 0 nil))
;;; NOT WORKING!
(define (compare-queens n)
  (let ((orig (time-proc queens n))
        (altd (time-proc alt-queens n)))
    (/ altd orig))
  (/ (time-proc alt-queens n) (time-proc queens n)))
(define (avg-compare-queens n tests)
  (define (iter count result)
    (if (> count tests)
        result
        (iter (+ count 1)
              (+ result (compare-queens n)))))
  (/ (iter 1 0) tests))

;;------------------------------------------------------------------------------------------
;; Sec 2.2.4: Example
;; A PICTURE LANGUAGE
;; ------------------
;; We develop a simple language for drawing pictures that illustrates the power of data
;; abstraction and closure and exploits higher order procedures.
;; The language makes it easy to make picures built of repeated elements shifted and scaled.
;; The data objects will be represented as porcedures rather than list structures.
;; Just as cons, which satisfied the closure property, allows us to build arbitrarily complicated
;; list structure, The operations in our language, which also satisfy clodure, will allow us
;; to build arbitrarily complicated patterns.

;; In Sec 1.1, we emphasised the importance of understanding a language by focusing on its:
;; * primitives
;; * means of combination
;; * means of abstraction

;; There is only one kind of element in our language, called a painter, which draws a picture
;; that is shifted and scaled to fit in a parallelogram-shaped frame. eg, a primitive painter
;; called paint-wave makes a crude line drawing.
;; To combine images, we use various operations that construct new painters from given painters.
;; eg. the 'beside' operations takes two painters and produces a compound painter that draws the
;; first painter's image in the left half of the frame and the second painter's image in the right.
;; Some operations transform a simgle painter, eg. flip-vertical.
;; We would like to be able to abstract typical means of combining painters: since painters are
;; represented as procs, we can use the same methods of abstraction as on any other procedure,
;; including recursive methods.

;; *** graphics device defined at top of file - uncomment to open

;; A simple painter for testing
(define (cross-painter frame)
  (draw-line (frame-origin frame)
             (add-vect (frame-edge1 frame) (frame-edge2 frame)))
  (draw-line (add-vect (frame-origin frame) (frame-edge1 frame))
             (add-vect (frame-origin frame) (frame-edge2 frame))))

;; Connect a sequence of points
(define (connect-points points)
  (define (iter start rest)
    (cond ((null? rest) 0)
          ((not (pair? rest)) (draw-line start rest))
          (else (draw-iter start (car rest) (cdr rest)))))
  (define (draw-iter start end rest)
    (draw-line start end)
    (iter end rest))
  (if (not (pair? points))
      0
      (iter (car points) (cdr points))))

;; star-painter - a pentacle painter for testing
(define (star-painter frame)
  (let ((O (frame-origin frame))  ;  locals copied from frame-vectors
        (e1 (frame-edge1 frame))
        (e2 (frame-edge2 frame))
        (A (add-vect O (frame-edge1 frame)))
        (B (add-vect O (frame-edge2 frame)))
        (me1 (scale-vect 0.5 (frame-edge1 frame)))
        (me2 (scale-vect 0.5 (frame-edge2 frame))))
        (let ((MidOB (add-vect O me2))
              (MidAC (add-vect A me2))
              (MidBC (add-vect B me1)))
          (connect-points (list O MidBC A MidOB MidAC O)))))

;; [ex 2.46] Implementation of (point) vectors in R^2
;; SICP does not specify whether they mean actual pairs or 2-lists
;; I will use pairs (x.y) since the length is fixed
(define (make-vect x y) (cons x y))
(define (vect-xord v) (car v))
(define (vect-yord v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (vect-xord v1) (vect-xord v2))
             (+ (vect-yord v1) (vect-yord v2))))
(define (scale-vect s v)
  (make-vect (* s (vect-xord v))
             (* s (vect-yord v))))
(define (sub-vect v1 v2) (add-vect v1 (scale-vect -1 v2)))

(define (midpoint-vect x y)  ;  mid-point of 2 point vectors
  (scale-vect 0.5 (add-vect x y)))

;; [ex 2.47] Implementation of frames
(define (alt-make-frame origin edge1 edge2)  ;  1st implementation - lists
  (list origin edge1 edge2))
(define (alt-frame-origin f) (car f))
(define (alt-frame-edge1 f) (cadr f))
(define (alt-frame-edge2 f) (caddr f))
;; 2nd implementation - pairs. I prefer this since the length of seq is fixed.
;; Also, a frame splits neatly into the origin and a pair of edges
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (frame-origin f) (car f))
(define (frame-edges f) (cdr f))
(define (frame-edge1 f) (car (frame-edges f)))
(define (frame-edge2 f) (cdr (frame-edges f)))

;(define (frame-vectors frame)  ;  returns absolute & relative vectors to significant points
;  (let* ((O (frame-origin frame))  ;  alternate let syntax allows cumulative definitions
;         (e1 (frame-edge1 frame))  ;  Capital letters represent points (position vectors)
;         (e2 (frame-edge2 frame))  ;  lowercase names are relative vectors.
;         (d1 (add-vect e1 e2))  ;  relative vectors for sides: e1 (OA) , e2 (OB)
;         (d2 (sub-vect e2 e1))  ;  relative vectors for diagonals: d1 (OC) , d2 (AB)
;         (A (add-vect O e1))       ;  vertices: O (origin of frame), A and B at he other
;         (B (add-vect O e2))       ;  end of e1 and e2 (respectively), and C opposite O.
;         (C (add-vect O
;                      (add-vect e1 e2)))
;         (me1 (scale-vect 0.5 e1))  ;  1/2*e1 - vector from O to mid-point of OA
;         (me2 (scale-vect 0.5 e2))  ;  1/2*e2 - vector from O to mid-point of OB
;         (md1 (scale-vect 0.5 d1))  ;  vector from O to X (mid-point of diagonals OC,AB)
;         (md2 (scale-vect 0.5 d2))   ;  vector from A to X (mid-point of diagonals OC,AB)
;         (MidOA (add-vect O me1))  ;  absolute (position) vector to mid-point of OA
;         (MidOB (add-vect O me2))  ;  absolute (position) vector to mid-point of OB
;         (MidAC (add-vect A me2))  ;  absolute (position) vector to mid-point of AC
;         (MidBC (add-vect B me1))  ;  absolute (position) vector to mid-point of BC
;         (X (add-vect O md1)))  ;  absolute (position) vector to X (centre of frame)
;    (list O A B C X MidOA MidOB MidAC MidBC e1 e2 d1 d2  me1 me2 md1 md2)))

;; TODO: Code a general 'vector-drawing' painter framework. A specific painter should be defined
;; by a sequence of line segments in the unit square(1). Transformation functions (x-expand/contract,
;; y-expand/contract, shear, translate, rotate) implement the general painter functionality - 
;; possibly by means of a factory-function accepting sequences of line-segments and returning fully
;; functional painter functions.
;; Note(1): Another utility function could convert a sequence of line-segments in an arbitrary
;; rectangle to the 'canonical representation' in the unit square

;; frame-coord-map returns a function for a given frame which will map vectors in the unit square
;; to vectors in that frame.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (frame-origin frame)
              (add-vect (scale-vect (vect-xord v)
                                    (frame-edge1 frame))
                        (scale-vect (vect-yord v)
                                    (frame-edge2 frame))))))

;; PAINTERS
(define (segments->painter segment-list)
  (lambda (frame)  ; return a proc taking a frame as argument, defined as follows
    (for-each (lambda (segment)
                (draw-line ((frame-coord-map frame) (start-segment segment))
                           ((frame-coord-map frame) (end-segment segment))))
              segment-list)))
;; [ex 2.48] implementation of segments
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (segment-start segment)
  (car segment))
(define (segment-end segment)
  (cdr segment))

;; [ex 2.49] Primitive painters.
;; (a) painter: frame outline
(define paint-frame
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                           (make-segment (make-vect 0 1) (make-vect 1 1))
                           (make-segment (make-vect 1 1) (make-vect 1 0))
                           (make-segment (make-vect 1 0) (make-vect 0 0)))))
 ;; (b) painter: paint-x
(define paint-x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                           (make-segment (make-vect 0 1) (make-vect 1 0)))))
;; (c) paint-diamond
(define paint-diamond
  (segments->painter (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
                           (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
                           (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
                           (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))
;; (d) paint-wave
;; Connect a sequence of points
(define (points->seglist pointlist)
  (define (iter start rest result)
    (if (null? rest)
        result
        (iter (car rest)
              (cdr rest)
              (append result
                      (list (make-segment start (car rest)))))))
  (iter (car pointlist) (cdr pointlist) (list)))
;; [aside] version that closes the figure at the end
(define (points->closed-seglist pointlist)
  (let ((end-points (append (cdr pointlist) (list (car pointlist)))))
    (map make-segment pointlist end-points)))
(define pointlists-wave  ; define the 5 connected regions as lists of points, assemble into list.
  (let ((v make-vect))
    (list (list (v .6 1) (v .65 .9) (v .6 .8) (v .7 .8) (v 1 .4))
          (list (v 1 .2) (v .6 .6) (v .8 0))
          (list (v .6 0) (v .5 .4) (v .4 0))
          (list (v .2 0) (v .4 .6) (v .2 .4) (v 0 .6))
          (list (v 0 .8) (v .2 .6) (v .3 .8) (v .4 .8) (v .35 .9) (v .4 1))
          (list (v .4 .88) (v .5 .8) (v .6 .88)))))
(define paint-wave
  (segments->painter
   (accumulate append
               (list)
               (map points->seglist
                    pointlists-wave))))

;; Transforming painters: painter operations work by creating a new painter which simply calls the
;; original painter on a transformed frame. They are based on the transform-painter operation, which
;; takes as arguments a painter and 3 point-vectors specifying a frame-transformation (the 3 points
;; give the transformed values of the origin, edge1 and edge2, for the unit square).
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame) ; return proc taking frame as arg, defined as follows:
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin ; call orig painter on transformed frame
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))
;; we can now easily define painter transformations
(define (flip-vert painter)
  (transform-painter painter ; orig painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new corner of edge1
                     (make-vect 0.0 0.0))) ; new corner of edge2
                     ; notice x-values of corners are the same but y-vals are swapped
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
;; rotate90 "rotates a frame 90 deg ccw - only a pure rotation for square frames
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;; means of combination: frame transformation is also the key to defining painter combining operations.
(define (beside painter1 painter2)
  (let ((paint-left (transform-painter painter1
                                       (make-vect 0.0 0.0)
                                       (make-vect 0.5 0.0)
                                       (make-vect 0.0 1.0)))
        (paint-right (transform-painter painter2
                                        (make-vect 0.5 0.0)
                                        (make-vect 1.0 0.0)
                                        (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))
;; [ex 2.50] flip-horiz, rotate180, rotate270
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;; [ex 2.51] below
(define (below1 painter1 painter2)
  (let ((paint-bottom (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.0 0.5)))
        (paint-top (transform-painter painter2
                                      (make-vect 0.0 0.5)
                                      (make-vect 1.0 0.5)
                                      (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))
(define (below painter1 painter2)
  (lambda (frame)
    ((rotate90 (beside (rotate270 painter1)
                      (rotate270 painter2)))
     frame)))

;; Back to material at beginning of section
(define wave2 (beside paint-wave (flip-vert paint-wave)))
;(define wave4 (below wave2 wave2))
;; abstract wave4 pattern above
(define (flipped-pairs1 painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
;; re-implement wave4 in terms of flipped-pairs1
(define wave4 (flipped-pairs1 paint-wave))
;; we can also implement recursive operations
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;; [ex 2.44] up-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;; corner-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;; square-limit
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;; square-of-four - HO painter proc takes as args four painter operations and returns an op which
;; takes a base painter and returns a painter which divides a frame in four and applies an op to each quarter.
(define (square-of-four tl tr bl br) ; 4 ops named by quadrant they are applied in: tl= top left, etc
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;; redefine flipped-pairs, square-limit using square-of-four
(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))
(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert)))
    (combine4 (corner-split painter n))))
;; [ex 2.45] split
(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  iter)
;; The picture language illustrates the critical ideas we've ecountered about abstraction with procedures
;; and data. The fundamental data abstractions, painters, are implemented as procedures, allowing the
;; language to handle different drawing capabilities in a uniform way. The means of combination satisfy the
;; closure property, allowing us to easily build arbitrarily nested, complex designs. And, all the tools for
;; abstracting procedures can be used to abstract the means of combination of painters.
;; The language also illlustrates another important idea: the approach of "stratified design": the notion
;; that a complex system should be structured as a sequence of levels using a sequence of languages.
;; Each level is constructed by combining parts regarded as primitives at that level, and the parts constructed
;; at each level are the primitives of the next level. At each level, the language used has its primitives,
;; means of combination and means of abstraction.
;; Stratified design helps make programs "robust".
;; Robustness means that a small change in specification will result in a small change in the program.
;; For instance, suppose we wanted to change the image produced by (square-limit paint-wave)
;; We could work at the lowest level to change the detailed appearance of the paint-wave element.
;; We could work at the middle level to change the way corner-split replicates the paint-wave element.
;; We could work at the highest level to change the eay square-limit arranges the 4 copies of the corner.

;; [ex 2.52] make changes to (square-limit paint-wave) working at each of the levels above.
;; (a) change paint-wave element
;;     - add (list (v .4 .88) (v .5 .8) (v .6 .88)) to list of lists of points in pointlists-wave to add smile.
;; (b) modify corner-split
(define (alt-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (alt-corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;; (c) modify square-limit
(define (alt-square-limit painter n)
  (let ((quarter (alt-corner-split painter n)))
    (let (( half (beside quarter (flip-horiz quarter))))
      (below half (flip-vert half)))))


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
; We will use the 'wishful thinking' method of top-down design
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
                             1
                             0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp)
                                                                                (- (exponent exp) 1)))
                                             (deriv (base exp) var)))
        (else (error "unknown expression type -- DERIV" exp))))

; representation of algebraic expressions: a particularly simple way to represent algebraic expressions
; is to use the LISP paenthesized prefix notation, with the symbols '+ and '* for addition and multiplication
; and symbols like 'x for variables.

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x)
  (and (pair? x)  ;  note: I would have used list? but I'll leave it like this
       (eq? (car x) '+)))  ; with proper lists, allows an empty sum '(+)
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; redefine make-sum and make-product to include some simplification rules
; these procs use the proc =number? - I'm not sure why this is used instead of =
; Apart from the specific arity, I can't even see any differece: = only takes numbers as args.
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

; [ex 2.56] exponentiation rule - added to deriv
(define (make-exponentiation b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        (else (list '** b n))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))
; [ex 2.57] extend sum, product to accept omre than 2 operands
; make the changes in the implementations of sum, product so that deriv doesn't have to be changed.
; The addend/multiplier will be the first operand. The augend/multiplicand will be the sum/product
; of all remaining operands.

(define (augend s)
  (let ((rest (cddr s)))
    (cond ((not (pair? rest)) 0)
          ((and (null? (cdr rest)) (number? (car rest))) (car rest))
          ((and (null? (cdr rest)) (symbol? (car rest))) (car rest))
          (else (append (list '+) rest)))))
(define (make-sum a . aug)
  (define (iter a1 aug result)
    (if (null? aug)
        (append result (list a1))
        (let ((a2 (car aug))
              (rest (cdr aug)))
          (cond ((=number? a1 0) (iter a2 rest result))
                ((=number? a2 0) (iter a1 rest result))
                ((and (number? a1) (number? a2)) (iter (+ a1 a2) rest result))
                (else (iter a2 rest (append result (list a1))))))))
  (cons '+ (iter a aug '())))

(define (multiplicand p)
  (if (null? (cddr p))
      1
      (cons '* (cddr p))))
(define (make-product m1 . rest)
  (define (iter m1 rest result)
    (cond ((null? rest) (append result (list m1)))
          ((=number? m1 0) 0)
          ((=number? m1 1) (iter (car rest) (cdr rest) result))
          ((=number? (car rest) 1) (iter m1 (cdr rest) result))
          ((and (number? m1) (number? (car rest))) (iter (* m1 (car rest)) (cdr rest) result))
          (else (iter (car rest) (cdr rest) (append result (list m1))))))
  (cons '* (iter m1 rest (list))))
; TODO: make these procs return neater results.

; [ex 2.58] Infix operators
; [part a] change +,* to binary infix notation where all expressions are fully parenthesized.
