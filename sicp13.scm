(load "c:/code/scheme/sicp/sicp12.scm")

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (identity x) x)
(define (inc x) (+ x 1))

;; sum calculates the sum of a series from a to b,
;; where the fn (term x) gives the form of a term
;; and the fn (next x) transforms x(i)->x(i+1)
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-integers a b)
  (sum identity a inc b))
;; pi-sum computes the sum from a to b of the series
;; 1/(1*3)+1/(5*7)+1/(9*11)+..., whose limit is pi/8
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x
            (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;; numerical integration using the formula
;; integral f(x)|a to b = (f(a+0.5dx)+f(a+1.5dx)+...)dx
(define (numintegral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx
     (sum f
          (+ a (/ dx 2))
          add-dx
          b)))
(define (simpsons f a b n)
  (define h (/ (+ a b) n))
  (define (next x) (+ x (* 2.0 h)))
  (* (/ h 3.0)
     (+ (f a)
        (* 4.0
           (sum f (+ a h) next (- b h)))
        (* 2.0
           (sum f (next a) next (- b h)))
        (f b))))

;; sum revised as an iteration
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a)
                 result))))
  (iter a 0))

(define (product factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (factor a)
                 result))))
  (iter a 1))

;; factorial procedure - only for natural numbers
(define (factorial n)
  (product identity 1 inc n))

;; wallis' formula: pi/2= (2*2/1*3)(4*4/3*5)(6*6/5*7)...
(define (wallis n)
  (define (next x) (+ x 2.0))
  (define (factor x)
    (/ (square (+ x 1.0))
       (* x (+ x 2.0))))
  (* 2.0 (product factor 1.0 next n)))

;; recursive version of product
(define (recursive-prod factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (recursive-prod factor
                         (next a)
                         next
                         b))))

;; accumulate - generalization of sum and poduct
;; an accumulator combines the terms of a sequence to obtain a result.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))
;; iterative version of accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a)
                        result))))
  (iter a null-value))
;; filtered-accumulate - as above, but only terms based on values
;; satisfying filter are accumulated.
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a)
                            (combiner (term a)
                                      result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (divides? poss-factor n)
  (= 0 (remainder n poss-factor)))
(define (least-factor n)
  (define (iter i)
    (cond ((> i (sqrt n)) n)
          ((divides? i n) i)
          (else (iter (+ i 1)))))
  (iter 2))
(define (prime? n)
  (= n (least-factor n)))

(define (sum-prime-sq a b)
  (filtered-accumulate + prime? 0 square a inc b))
(define (relative-primes n) ;all natural nums <n, rel. prime to n
  (define (prime-to-n a) (= 1 (gcd a n)))
  (filtered-accumulate * prime-to-n 1 identity 1 inc (- n 1)))

;; redefine pi-sum, numintegral using lambda
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))
(define (numintegral f a b dx)
  (* dx (sum f
             (+ a (/ dx 2))
             (lambda (x) (+ x dx))
             b)))

(define (average a b) (/ (+ a b) 2))
(define (find-zero f neg-point pos-point)
  (define (close-enough? a b) (< (abs (- a b)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (cond ((positive? (f midpoint)) (find-zero f neg-point midpoint))
              ((negative? (f midpoint)) (find-zero f midpoint pos-point))
              (else midpoint)))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value)) (find-zero f a b))
          ((and (positive? a-value)
                (negative? b-value)) (find-zero f b a))
          (else (error "Values are not of opposite sign" a b)))))

;; fixed-point procedure makes repeated fn applications until succesive values
;; remain 'constant' (within a given tolerance)
;; note: this fixed-point search will not always converge on the answer
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))
;; square roots: to find the sqrt of x we solve the eqn y^2=x
;; or, if x!=0, y=x/y
;; attempt to find sqrt by fixed-point procedure--
(define (fpsqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))  ;does not converge
;; this oscillates between y1=first-guess and x/y1
;; note that sqrt(x) is always between y1 and x/y1 so--
(define (estsqrt x)
  (fixed-point (lambda (y) (average y
                                    (/ x y))) 1.0))
;; this is called average-dampening

(define (average-damp f)
  (lambda (x) (average x
                       (f x))))
(define (damped-fixed-point f first-guess)
  (fixed-point (average-damp f) first-guess))
(define phi-est
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (fixed-point-display f first-guess)
  (define tolerance 0.00001)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (display first-guess)
  (newline)
  (let ((next (f first-guess)))
    (if (good-enough? first-guess next)
        next
        (fixed-point-display f next))))

;; k-term continued fraction n1/(d1+n2/(d2+n3/(...nk/dk))..)
(define (cont-frac n d k)
  (define (recurs n d k i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (recurs n d k (+ i 1))))))
  (iter n d k 1))
;; iterative version
(define (cont-frac n d k)
  (define (iter n d k i cont)
    (if (< i 1)
        cont
        (iter n
              d
              k
              (- i 1)
              (/ (n i)
                 (+ (d i) cont)))))
  (iter n d k k 0))

;; calculates e-2 using Euler's cont frac, where
;; ni are all 1, and di are 1,2,1,1,4,1,1,6,..
(define (emin2 k)
  (define (d i)
    (let ((j (+ i 1)))
      (if (divides? 3 j)
          (/ j 3)
          1)))
  (cont-frac (lambda (x) 1.) d k))
;; Lambert's tangent representation as a cont frac:
;; tan r = x/(1-x^2/(3-x^2/(5-...)..)))
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (- (* 2 i) 1.))
  (cont-frac n d k))


;; rewritten estsqrt using average-damp proc
(define (est-sqrt x )
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.))
;; cube root estimated with avg-damped fixed-point
;; for eqn y=x/y^2 (equivalent to y=x^3 for x!=0)
(define (cube-root x)
  (damped-fixed-point (lambda (y) (/ x (square y))) 1.))


;; The square-root approximation method above is a special case of:
;; Newton's Method for finding roots of a differentiable fn:
;; if g(x) is differentiable, then a soln to g(x)=0 is a fixed point
;; of f(x) = x - g(x)/g'(x)
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))
(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
;; est-sqrt in terms of Newton's Method
(define (est-sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.))

;; Both Newton's Method and the average-dampening method of estimating
;; sqrt work by transforming a given fn, then finding a fixed point of
;; the transform. We can write a general proc for this pattern:
(define (fixed-pt-of-transform g transform guess)
  (fixed-point (transform g) guess))
;; define est-sqrt in terms of fixed-pt-of-transform
(define (est-sqrt x)
  (fixed-pt-of-transform (lambda (y) (/ x y))
                         average-damp
                         1.0))
;; or in terms of Newton's method and fixed-pt-of-transform
(define (newt-sqrt x)
  (fixed-pt-of-transform (lambda (y) (- (square y) x))
                         newton-transform
                         1.0))
;; cubic - a transform used with newtons-method to find zeroes
;; of eqns of the form x^3+ax^2+bx+c
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;; double f applies f twice
(define (double f)
  (lambda (x) (f (f x))))
;; composition of f and g: ((compose f g) x) is f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))
;; repeated f composes f with itself (n-1) times
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((repeated f (- n 1)) x)))))
;; iterative version
(define (repeated f n)
  (define (iter f n i result)
    (if (= i n)
        result
        (iter f
              n
              (+ i 1)
              (compose f result))))
  (iter f n 0 identity))

;; smooth - smoothes a fn at pt x by averaging x-dx,x and x+dx
;; returns smoothed fn.
(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;; the nth root of x is given by y^n=x, ie. y=x/y^(n-1)
(define (nth-rt-fp-fn n x)
  (lambda (y) (/ x (expt y (- n 1)))))
(define (multi-avg-damp n)
  (repeated average-damp n))
(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))
(define (nth-pow n)
  (lambda (x) (expt x n)))
;; test-root uses the fixed-pt method to find the nth root of testval, with
;; numdamps applications of damping, and exponentiates the result to test its accuracy.
;; returns boolean.
(define (test-root n numdamps testval)
  (let ((tolerance 0.01))
    (close-enough? testval
                   ((nth-pow n) (fixed-pt-of-transform (nth-rt-fp-fn n testval)
                                                       (multi-avg-damp numdamps)
                                                       1.0))
                   tolerance)))
;; TODO: Write count-damps to automatically determine how many applications of avg-damping
;; is needed to make the search converge for the nth-root. Since failed procs go into endless
;; loops, this will require a new fixed-pt proc with a 'safety clause' that checks for
;; divergence (either directly, or by the number of iterations/ amount of time elapsed
;; without success)

;; NOTE: Manual tests suggest that the number of levels of avg-damping needed for convergence
;; of the nth-root search is floor(log-base-2 (n))
;; With a testval of 4:
;; 1 avg-damp was sufficient for n=2,3
;; 2 avg-damps needed for n=4-7
;; 3 avg-damps needed for n=8-15
;; 4 avg-damps needed for n=16...test finished.
(define (logb2 x)
  (/ (log x) (log 2)))
(define (nth-root n x)
  (fixed-pt-of-transform (nth-rt-fp-fn n x)
                         (multi-avg-damp (floor(logb2 n)))
                         1.0))

;; iterative-improve - Iterative improvement is a general numerical method used several times
;; in this chapter. It starts with an initial guess and successively improves it until it is
;; good enough. This procedure takes 2 args: a boolean proc good-enough? and a proc improve which
;; improves the estimate for the next iteration
(define (iterative-improve good-enough? improve)
  (define (iter good-enough? improve guess)
    (if (good-enough? guess)
        guess ; (improve guess) seems better, but wording of instructions implies guess
        (iter good-enough? improve (improve guess))))
  (lambda (guess) (iter good-enough? improve guess)))
;; sqrt-iter from sec1.17 in terms of iterative-improve
(define (sqrt-iter x guess)
  ((iterative-improve (lambda (guess) (< (abs (- x (square guess))) 0.001))
                      (lambda (guess) (average guess (/ x guess)))) guess))
;; fixed-point in terms of iterative-improve
(define (fixed-point-ii f guess)
  (let ((tolerance 0.00001))
    ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
                        (lambda (guess) (f guess)))
     1.0)))