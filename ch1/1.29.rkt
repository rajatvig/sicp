#lang sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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
