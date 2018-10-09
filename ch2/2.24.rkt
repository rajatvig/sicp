#lang sicp

(define (for-each l f)
  (let ((rest (cdr l)))
    (f (car l))
    (if (not(null? rest))
       (for-each rest f)
       #t)))
