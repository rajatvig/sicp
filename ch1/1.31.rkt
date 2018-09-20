#lang sicp
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (factorial n)
  (define (identity x) x)
  (product identity 1 inc n))

(define (pi n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (exact->inexact (* 4 (product term 1 inc n))))

(define (product-i term a next b)
  (define (product-iter a acc)
    (if (> a b)
        acc
        (product-iter (next a) (* acc (term a)))))
  (product-iter a 1))

(define (pi-i n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (exact->inexact (* 4 (product-i term 1 inc n))))
