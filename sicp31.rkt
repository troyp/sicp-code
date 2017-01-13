#! /opt/racket/bin/drracket
#lang racket

;; *********************************
;; *                               *
;; *  MODULARITY, OBJECTS & STATE  *
;; *                               *
;; *********************************

;; ================================
;; 3.1. Assignment and Local State.
;; ================================

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-balance
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W2 70)
(W2 40)
(W1 40)

;;; added test for positive amount (useless checking balance on withdraw if deposit takes a negative)
;(define (make-account balance)
;  (define (withdraw amount)
;    (cond ((<= amount 0) "Invalid withdrawal")
;          ((>= balance amount) (set! balance (- balance amount))
;                               balance)
;          (else "Insufficient funds")))
;  (define (deposit amount)
;    (cond ((<= amount 0) "Invalid deposit")
;          (else (set! balance (+ balance amount))
;                balance)))
;  (define (dispatch m)
;    (cond ((eq? m 'withdraw) withdraw)
;          ((eq? m 'deposit)  deposit)
;          (else (error "Unknown request --MAKE-ACCOUNT"
;                       m))))
;  dispatch)
;
;(define acc (make-account 100))
;((acc 'withdraw) 50)
;((acc 'withdraw) 60)
;((acc 'deposit)  40)
;((acc 'withdraw) 60)

;; EX.3.1.
(define (make-accumulator value)
  (lambda (amount)
    (set! value (+ value amount))
    value))
(define A (make-accumulator 5))
(A 10)
(A 10)

;; EX.3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (set! count (+ count 1)) (f m))))))
(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls)
(s 'reset-count)
(s 'how-many-calls)

;;; EX.3.3.
;(define (make-account balance acc-pword)
;  (define (withdraw amount)
;    (cond ((<= amount 0) "Invalid withdrawal")
;          ((>= balance amount) (set! balance (- balance amount))
;                               balance)
;          (else "Insufficient funds")))
;  (define (deposit amount)
;    (cond ((<= amount 0) "Invalid deposit")
;          (else (set! balance (+ balance amount))
;                balance)))
;  (define (dispatch pword m)
;    (cond ((not (eq? acc-pword pword)) (lambda (amount) "Invalid password"))
;          ((eq? m 'withdraw) withdraw)
;          ((eq? m 'deposit)  deposit)
;          (else (error "Unknown request --MAKE-ACCOUNT"
;                       m))))
;  dispatch)
;(define acc (make-account 100 'secret-password))
;((acc 'secret-password 'withdraw) 40)
;((acc 'some-other-password 'deposit) 50)

;; EX.3.4
;; It's not clear in the spec exactly when a password attempt is assessed and
;; "counted" (either resetting or incrementing #tries); nor when the police
;; are called.
;; In this solution, a password attempt counts and police are called at the
;; time a deposit/withdrawal method is returned, but the invalid password
;; message is only returned when that method is actually called.
(define (make-account balance acc-pword)
  (define MAX-TRIES 7)
  (define tries 0)
  (define (withdraw amount)
    (cond ((<= amount 0) "Invalid withdrawal")
          ((>= balance amount) (set! balance (- balance amount))
                               balance)
          (else "Insufficient funds")))
  (define (deposit amount)
    (cond ((<= amount 0) "Invalid deposit")
          (else (set! balance (+ balance amount))
                balance)))
  (define (dispatch pword m)
    (if (eq? acc-pword pword)
        (begin (set! tries 0)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit)  deposit)
                     (else (error "Unknown request --MAKE-ACCOUNT"
                                  m))))
        (begin (set! tries (+ tries 1))
               (when (>= tries MAX-TRIES) (call-the-cops))
               (lambda (amount) "Invalid password"))))
  dispatch)
(define (call-the-cops) (displayln "Cops are on their way, scumbag!"))
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
(for ([i (in-range 7)])
  (displayln ((acc 'some-other-password 'deposit) 50)))

;; Benefits of Introducing Assignment.
;; -----------------------------------
(define RANDOM-INIT 1)
(define (rand-update seed)
  (let ((m (expt 2 64))
        (a 6364136223846793005)
        (c 1442695040888963407))
    (((a . * . seed) . + . c) . modulo . m)))
(define rand
  (let ((x RANDOM-INIT))
    (lambda ()
      (set! x (rand-update x))
      x)))

