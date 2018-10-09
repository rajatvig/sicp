#lang sicp

(define (cc amount list-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? list-coins)) 0)
        (else (+ (cc amount
                     (cdr list-coins))
                 (cc (- amount
                        (car list-coins))
                     list-coins)))))

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))
