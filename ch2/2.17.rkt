#lang sicp

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (list-ref l i)
  (if (= i 0)
      (car l)
      (list-ref (cdr l) (- i 1))))

(define (square l)
  (map (lambda(x) (* x x)) l))

(define (last l)
  (let ((elem (cdr l)))
    (if (null? elem)
        (car l)
        (last (cdr l)))))
