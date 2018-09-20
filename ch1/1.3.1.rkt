#lang sicp
(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cubes a b)
  (define (inc n) (+ n 1))
  (sum cube a inc b))

(define (sum-integers a b)
  (define (inc n) (+ n 1))
  (define (identity x) x)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (sum pi-term a pi-next b)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
