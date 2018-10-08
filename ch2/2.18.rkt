#lang sicp

(define (reverse l)
  (if (null? l)
      '()
      (cons (reverse (cdr l)) (car l))))
