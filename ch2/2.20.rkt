#lang sicp

(define (same-parity . l)
  (define (parity l test)
    (if (null? l)
        '()
        (if (test (car l))
            (cons (car l) (parity (cdr l) test))
            (parity (cdr l) test))))
  (if (even? (car l))
      (parity l even?)
      (parity l odd?)))

;;              ;
