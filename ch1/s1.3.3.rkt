#lang sicp
(define tolerance 0.0001)

(define (average x y) (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- y x)) tolerance))

(define (search f a b)
  (let ((mid (average a b)))
    (if (close-enough? a b)
        mid
        (let ((test-val (f mid)))
          (cond ((positive? test-val) (search f a mid))
                ((negative? test-val) (search f mid b))
                (else mid))))))

(define (half-interval f a b)
  (let ((a-val (f a))
    (b-val (f b)))
  (cond ((and (negative? a-val) (positive? b-val)) (search f a b))
        ((and (negative? b-val) (positive? a-val)) (search f b a))
        (else (error "invalid" a b)))))

(define (fixed-point f guess)
  (define (try x)
    (let ((next (f x)))
      (if (close-enough? next x)
          next
          (try next))))
  (try guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))

;; 1.35
(define (phi)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

;; 1.36
(define (xx y)
  (fixed-point (lambda (x) (/ (log y) (log x))) 2.0))

;; 1.37
(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (cont-frac-i n d k)
  (define (cont-frac-iter x acc)
    (if (= x 0)
        acc
        (cont-frac-iter (- x 1) (/ (n x) (+ acc (d x))))))
  (cont-frac-iter k 0))

(define (phi-f k)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

;; 1.38
(define (e k)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (d i)
    (if (divides? 3 (+ 1 i))
        (* 2 (/ 3 (+ 1 i)))
        1))
  (+ 2 (cont-frac-i (lambda (i) 1.0) d k)))

;; 1.39
(define (tan x k)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (- (* 2 i) 1))
  (cont-frac-i n d k))
