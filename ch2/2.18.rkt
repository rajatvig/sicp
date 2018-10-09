#lang sicp

(define (reverse l)
  (define (iter l a)
    (if (null? l)
        a
        (iter (cdr l) (cons (car l) a))))
  (iter l '()))
