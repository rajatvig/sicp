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

;; 1.40

(define (cubic a b c)
  (fixed-point-of-transform (lambda (x) (+ (cube x) (* x x a) (* x b) c)) newton-transform 1.0))

;; 1.41

(define (double f)
  (lambda (x) (f (f x))))

;; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; 1.43

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
  )

;; 1.44

(define (smoothing f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (smoothing-nfold f n)
  (repeated smoothing n) f)

;; 1.45

(define (cubic-rt x)
  (fixed-point-of-transform (lambda (y) (- (cube y) x)) newton-transform 1.0))

(define (cubic-rt-d x)
  (fixed-point-of-transform (lambda (y) (/ x (square y))) average-damp 1.0))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (- (expt y n) x)) newton-transform 1.0))

(define (nth-root-d x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1)))) (repeated average-damp (floor (log n))) 1.0))
