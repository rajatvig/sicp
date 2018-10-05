#lang sicp

(define (pair a b)
  (cons a b))

(define (make-point x y)
  (pair x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-segment p1 p2)
  (pair p1 p2))

(define (mid-segment s1)
  (define (mid pf)
    (/ (+ (pf (start-segment s1)) (pf (end-segment s1)))) 2)
  (make-point
    (mid x-point)
    (mid y-point)))

(define (make-rect seg h)
  (define (g fp fs) (fp (fs seg)))
  (let ((x1 (g x-point start-segment))
        (y1 (g y-point start-segment))
        (x2 (g x-point end-segment))
        (y2 (g y-point end-segment)))
    (cons (make-segment
           (make-point x1 y1)
           (make-point x2 y2))
          (make-segment
           (make-point (+ x1 h) (+ y1 h))
           (make-point (+ x2 h) (+ y2 h))))))

(define (width r)
  (let ((s (car r)))
    (- (x-point (end-segment s)) (x-point (start-segment s)))))

(define (height r)
  (let ((s1 (car r))
        (s2 (cdr r)))
    (- (y-point (start-segment s2)) (y-point (start-segment s1)))))

(define (area r)
  (* (width r) (height r)))

(define (perimeter r)
  (* (+ (width r) (height r)) 2))
