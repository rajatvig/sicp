#lang sicp

(define (make-interval c p)
  (let ((h (+ c (/ (* c p) 100)))
        (l (- c (/ (* c p) 100))))
    (cons l h)))
