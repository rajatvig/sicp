#lang sicp
(define (cube x)
  (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* (y k) 2))
          (else (* (y k) 4))))
  (define (inc n) (+ n 1))
  (/ (* h (sum term 0 inc n)) 3))

(define (sum term a next b)
  (define (iter a r)
    (if (> a b)
        r
        (iter (next a) (+ (term a) r))))
  (iter a 0))
