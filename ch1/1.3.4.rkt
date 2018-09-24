#lang sicp

(define dx 0.00001)

(define (close-enough? x y)
  (< (abs (- y x)) dx))

(define (fixed-point f guess)
  (define (try x)
    (let ((next (f x)))
      (if (close-enough? next x)
          next
          (try next))))
  (try guess))

(define (derive g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derive g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-f x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
