#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
        (neg (< d 0)))
    (cons (/ (if neg (- n) n) g) (/ (abs d) g))))

(define (denom rat)
  (cdr rat))

(define (numer rat)
  (car rat))

(define (add-rat x y)
  (make-rat (+ (* (denom y) (numer x))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (denom y) (numer x))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer y) (numer x))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (denom y) (numer x))
            (* (numer x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
