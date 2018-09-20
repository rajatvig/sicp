#lang sicp
(define (acc op f i term a next b)
  (if (> a b)
      i
      (if (f a)
          (op (term a) (acc op f i term (next a) next b))
          (acc op f i term (next a) next b))
      ))

(define (square x) (* x x))

(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (smallest-divisor n)
    (define (find-divisor t)
      (cond ((> (square t) n) n)
            ((divides? t n) t)
            (else (find-divisor (+ t 1)))))
    (find-divisor 2))
  (= n (smallest-divisor n)))

(define (sum-squares a b)
  (define (inc x) (+ x 1))
  (acc + prime? 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (define (filter x) (= (gcd x n) 1))
  (acc * filter 1 identity 1 inc n))
