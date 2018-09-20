#lang sicp
(define (acc op i term a next b)
  (if (> a b)
      i
      (op (term a) (acc op i term (next a) next b))))

(define (acc-i op i term a next b)
  (define (acc-iter a acc)
    (if (> a b)
        acc
        (acc-iter (next a) (op acc (term a)))))
  (acc-iter a i))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (acc-i * 1 identity 1 inc n))

(define (pi n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (exact->inexact (* 4 (acc * 1 term 1 inc n))))
