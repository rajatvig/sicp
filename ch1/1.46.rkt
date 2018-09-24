#lang sicp

(define dx 0.00001)

(define (iter-improve good? improve)
  (lambda (x)
    (define (iter g)
      (if (good? g)
          g
          (iter (improve g))))
    (iter x)))

(define (close-enough? x y)
  (< (abs (- y x)) dx))

(define (fixed-point f guess)
  ((iter-improve (lambda (x) (close-enough? x (f x))) f) guess))

(define (square x) (* x x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iter-improve good-enough? improve) 1.0))

(define (sqrt-f x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))
