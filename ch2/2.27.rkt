#lang sicp

(define (reverse l)
  (define (iter l a)
    (if (null? l)
        a
        (iter (cdr l) (cons (car l) a))))
  (iter l '()))

(define (d-reverse items)
  (define (iter l a)
    (cond ((null? l) a)
          ((pair? (car l)) (iter (cdr l) (cons (reverse (car l)) a)))
          (else (iter (cdr l) (cons (car l) a)))))
  (iter items '()))

(define x (list 1 2 3))
(define y (list 4 5 6))
(define z (list 7 8 9))
(define a 10)

(d-reverse (list x y z a))
(list 10 (list 9 8 7) (list 6 5 4) (list 3 2 1))

(define (q-reverse l)
  (reverse (map reverse l)))
