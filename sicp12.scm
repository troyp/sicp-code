;#lang scheme

(define (factorial n)
  (if (= n 1)
  1
  (* n
     (factorial (- n 1)))))

(define (permutation n r)
  (/ (factorial n)
     (factorial (- n r))))

(define (combination n r)
  (/ (permutation n r)
     (factorial r)))

;; Ackermann's Function: this computes "higher order eponentiation"
;; A(0,y) = 2+2+..+2 (y terms) = 2y
;; A(1,y) = 2*2*...*2 (y products) = 2^y
;; A(2,y) = 2^2^...^2 (y exponents)
;; A(3,y) = 2^2^......,^2 (2^2^...^2=A(2,y) exponents)
(define (ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ackermann (- x 1)
                         (ackermann x (- y 1))))))

;; Fibonacci Numbers - the nth Fibonacci number is the
;; closest integer to (phi^n)/sqrt(5)
;; where phi=(1+sqrt(5))/2 is the golden ratio (phi^2=phi+1)
;; There is an analytic solution for Fib(n)
;; if psi=(1-sqrt(5))/2
;; then Fib(n)=(phi^n-psi^n)/sqrt(5)
(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2)))))) 

(define (ifib n) (fib-iter 0 1 n))
(define (fib-iter i inext n-iter)
  (if (= n-iter 0)
      i
      (fib-iter inext (+ i inext) (- n-iter 1))))

(define (count-change amount)
  (coins-iter amount 5))
(define (coins-iter amount numdenoms)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= numdenoms 0)) 0)
        (else (+ (coins-iter amount (- numdenoms 1))
                 (coins-iter (- amount
                                (firstdenom numdenoms)) numdenoms)))))
(define (firstdenom numdenoms)
  (cond ((= numdenoms 1) 1)
        ((= numdenoms 2) 5)
        ((= numdenoms 3) 10)
        ((= numdenoms 4) 25)
        ((= numdenoms 5) 50)))

(define (pascal line element)
  (cond ((or (< line 1)
             (> element line)
             (< element 1)) 0)
        ((and (= line 1)
              (= element 1)) 1)
        (else (+ (pascal (- line 1) element)
                 (pascal (- line 1) (- element 1))))))


;; the Fibonacci transforms a<-a+b, b<-a are special cases of a more
;; general class of linear transform T(p,q):
;; a <- pa + qa + qb , b <- qa + pb
;; The set of such transforms is closed under composition.
;; The square of T(p,q) is:
;; a <- (p^2+2q^2+2pq)a + (q^2+2pq)b
;; b <- (q^2+2pq)a + (p^2+q^2)b
;; ie. p<-p^2+q^2 , q<-q^2+2pq
;; The Fibonacci transform is T(0,1)
(define (fastfib n)
  (fastfib-iter 1 0 0 1 n))
(define (fastfib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fastfib-iter a
                                     b
                                     (+ (square p)
                                        (square q))
                                     (+ (square q)
                                        (* 2 p q))
                                     (/ count 2)))
        (else (fastfib-iter (+ (* p a) (* q a) (* q b))
                            (+ (* q a) (* p b))
                            p
                            q
                            (- count 1)))))
(define (square x) (* x x))

;; exponentiation algorithms
(define (recursive-exp b n)
  (if (= n 0)
      1
      (* b (recursive-exp b (- n 1)))))
(define (iexp b n)
  (exp-iter b n 1))
(define (exp-iter base counter product)
  (if (= counter 0)
      product
      (exp-iter base (- counter 1) (* base product))))

(define (fast-exp base n)
  (cond ((= n 0) 1)
        ((even? n) (fast-exp (square base) (/ n 2)))
        (else (* base (fast-exp base (- n 1))))))
(define (even? n) (= (remainder n 2) 0))

;; fast iterative exponentiation algorithm
;; (defined using tail-recursion).
;; 'product' variable eventually holds result.
;; Note that (product)*(base)^n is state-invariant.
(define (fast-iexp base n)
  (fastexp-iter base n 1))
(define (fastexp-iter base n product)
  (cond ((= n 0) product)
        ((even? n) (fastexp-iter (square base) (/ n 2) product))
        (else (fastexp-iter base (- n 1) (* base product)))))

;; Euclid'a algorithm for GCD - O(log n)
;; We can show this using:
;; Lame's Theorem: If Euclid's algorithm requires k steps to
;; compute the GCD of a pair, then the smaller number of the pair
;; must be greater than or equal to the kth Fibonacci number.
(define (gcd-euclid a b)
  (if (= b 0)
      a
      (gcd-euclid b (remainder a b))))

(define (least-divisor n)
  (next-divisor n 2))
(define (next-divisor n candidate)
  (cond ((> (square candidate) n) n)
        ((divides? candidate n) candidate)
        (else (next-divisor n (next23 candidate)))))
(define (divides? poss-factor n)
  (= 0 (remainder n poss-factor)))
(define (prime? n)
  (= (least-divisor n) n))
(define (composite? n)
  (not (prime? n)))


;; expmod - calculates base^exponent (mod modulus)
(define (expmod base exponent modulus)
  (cond ((= exponent 0) 1)
        ((even? exponent) (remainder (square (expmod base
                                                     (/ exponent 2)
                                                     modulus))
                                     modulus))
        (else (remainder (* base (expmod base
                                         (- exponent 1)
                                         modulus))
                         modulus))))
;; Fermat Test - probabilistic algorithm to test for primality
;; in O(log n) time. Relies on Fermat's Little Theorem:
;; If p is prime and a<p (in Z+), then a^p = a (mod p)
;; fermat-test relies on a procedure random(n), which should
;; return a pseudo-random number between 0 and n-1.
(define (fermat-test-for-value a p)
  (= (expmod a p p) a))
(define (fermat-test n)
  (fermat-test-for-value (+ 1 (random (- n 1))) n))
(define (fastprime? candidate numtests)
  (cond ((= numtests 0) true)
        ((fermat-test candidate) (fastprime? candidate
                                             (- numtests 1)))
        (else false)))

;;; timed algorithm
;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (runtime)))
;(define (start-prime-test n initial-time)
;  (if (prime? n) (report-prime (- (runtime) initial-time))))
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))
;
;
;;; next-prime n - finds the first prime p>=n using prime?
;;; then tests it using timed-prime-test
;(define (next-prime n)
;  (if (prime? n)
;      (prime?time n)
;      (next-prime (+ n 1))))
;;; primetest-test - tests growth rate of time as n increases
;(define (primetest-test numtests)
;  (primetest-test-iter 100000 numtests 0 0))
;(define (primetest-test-iter startval numtests small-time large-time)
;  (newline)
;  (if (and (> large-time 0)
;           (> small-time 0))
;      (display (/ large-time small-time))
;      (display "no value"))
;  (if (> numtests 0)
;      (primetest-test-iter (* startval 10)
;                           (- numtests 1)
;                           large-time
;                           (next-prime startval))))
;(define (prime?time n)
;  (start-prime?time n (runtime)))
;(define (start-prime?time n initial-time)
;  (if (prime? n) (- (runtime) initial-time)))
;
;(define (search-for-primes startval)
;  (if (= (remainder startval 2) 0)
;      (primesearch-iter (+ startval 1) 3)
;      (primesearch-iter startval 3)))
;(define (primesearch-iter n numprimes)
;  (if (> numprimes 0)
;      (timed-prime-test n))
;  (if (> numprimes 0)
;       (if (prime? n)
;           (primesearch-iter (+ n 2) (- numprimes 1))
;           (primesearch-iter (+ n 2) numprimes))))

;; next23 n - used to increase efficiency of least-divisor by skipping evens>2
;; returns succeeding value in series 2,3,5,7,9,...
(define (next23 n)
  (if (= n 2)
      3
      (+ n 2)))

;;; time-proc proc n - times a procedure call with argument n
;;; TODO: generalize number of args
;(define (time-proc proc n)
;  (timed-proc-app proc n (runtime)))
;(define (timed-proc-app proc n start-time)
;  (display (proc n))
;  (- (runtime) start-time))


;; -----------------------------------------------------------------------------------------
;; These Miller-Rabin procedures are giving false negatives. TODO: fix!
;; I'm going to use a simpler version of expmod giving 0 on an M-R-test fail.
;; -----------------------------------------------------------------------------------------
;; mr-expmod - modified expmod used to implement Miller-Rabin test.
;; During each squaring step, it checks for a nontrivial square root of 1
;; ie. a number other than 1 or p-1 that gives 1 (mod p) when squared.
;; If such a root exists, then p is not prime. If p is not prime, then for
;; at least 1/2 the possible values of a, mr-expmod will reveal such a root.
;; mr-expmod returns a negative if it finds such a root.
(define (make-negative n)
  (if (positive? n)
      (- n)
      n))
;; mr-check - checks number a to see if it is a nontrivial sqrt of 1.
;; if so, it returns a negative result.
(define (mr-check a n)
  (if (and (= 1 (remainder (square a) n))
           (not (or (= a 1)
                    (= a (- n 1)))))
      #t
      #f))
(define (mr-expmod base exponent modulus)
  (mr-iter base exponent modulus #f))
(define (mr-iter base exponent modulus flag)
  (cond ((= exponent 0) (if flag
                            -1
                            1))
        ((even? exponent) (mr-iter-reduc base
                                         exponent
                                         modulus
                                         (mr-count2s exponent)
                                         #f))
        (else (* base (mr-iter base
                               (- exponent 1)
                               modulus
                               flag)))))
(define (mr-count2s exponent)
  (define (count-iter exponent count)
    (if (= 0 (remainder exponent 2))
        (count-iter (/ exponent 2)
                    (+ count 1))
        count))
  (count-iter exponent 0))
(define (mr-iter-reduc base exponent modulus num2s flag)
  (if (= num2s 0)
      (mr-iter base exponent modulus flag)
      (mr-iter-reduc (square base)
                     (/ exponent 2)
                     modulus
                     (- num2s 1)
                     (or flag (mr-check base modulus)))))
;; Miller-Rabin test for primality - accounts for Carmichael Numbers.
(define (miller-rabin-test-for-value a p)
  (= (mr-expmod a (- p 1) p) 1))
(define (miller-rabin-test n)
  (miller-rabin-test-for-value (+ 1 (random (- n 1))) n))
(define (mr-fastprime? candidate numtests)
  (cond ((= numtests 0) true)
        ((miller-rabin-test candidate) (mr-fastprime? candidate (- numtests 1)))
        (else false)))

;; -----------------------------------------------------------------------------------------
;; -----------------------------------------------------------------------------------------
;; Miller-Rabin Test - 2nd version
;;
(define (mr-emod base exponent modulus)
  (cond ((= exponent 0) 1)
        ((even? exponent) (mr-even base
                                   exponent
                                   modulus))
        (else (remainder (* base (mr-emod base
                                          (- exponent 1)
                                          modulus))
                         modulus))))
(define (mr-even base exponent modulus)
  (if (= (remainder (square base) modulus) 1)
      0
      (mr-emod (square base)
               (/ exponent 2)
               modulus)))
(define (mr-test-for-val a p)
  (if (= 1 (mr-emod a (- p 1) p))
      #t
      #f))
(define (mr-prime? candidate numtests)
  (cond ((= numtests 0) #t)
        ((mr-test-for-val (+ (random (- candidate 1)) 1) candidate) (mr-prime? candidate
                                                                               (- numtests 1)))
        (else #f)))
