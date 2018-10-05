#lang sicp

(define (cons a b)
  (lambda (x) (x a b)))

(define (car c)
  (c (lambda (a b) a)))

(define (cdr c)
  (c (lambda (a b) b)))
