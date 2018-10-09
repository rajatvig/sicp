#lang sicp

(define (square x)
  (* x x))

(define (square-list items)
  (define (iter-things l answer)
    (if (null? l)
        answer
        (iter-things (cdr l) (cons (square (car l)) answer))))
  (iter-things items nil))
