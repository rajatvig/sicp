#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (iter a f)
  (if (= (remainder a f) 0)
      (iter (/ a f) f)
      a))

(define (iter-count a f n)
  (if (= (remainder a f) 0)
      (iter-count (/ a f) f (+ n 1))
      n))

(define (car c)
  (iter-count (iter c 3) 2 0))

(define (cdr c)
  (iter-count (iter c 2) 3 0))
