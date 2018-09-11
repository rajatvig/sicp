(define pi 3.14)
(define radius 10)
(define area (* pi (* radius radius)))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (abs x) (if (< x 0) (- x) x))

(define (sqrt x)
  (define (sqrt-iter guess)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) 0.0001))
    (define (improve guess)
      (/ (+ guess (/ x guess)) 2))
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (new-if p t e)
  (cond (p t)
        (else e)))

(define (cubic x)
  (define (cubic-iter guess)
    (define (good-enough? guess)
      (< (abs (- (cube guess) x)) 0.0001))
    (define (improve guess)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (if (good-enough? guess)
        guess
        (cubic-iter (improve guess))))
  (cubic-iter 1.0))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(define (fib-2 n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib-2 (- n 1)) (fib-2 (- n 2))))
    )
  )

(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter (+ a b) a (- n 1))))
  (fib-iter 0 1 n))

(define (f3 n)
  (if (< n 3)
      n
      (+ (f3 (- n 1)) (* 2 (f3 (- n 2))) (* 3 (f3 (- n 3))))))

(define (f3-2 n)
  (define (f3-iter a b c n)
    (define (f3-op c b a)
      (+ a (* 2 b) (* 3 c)))
    (if (= n 0)
        a
        (f3-iter b c (f3-op a b c) (- n 1)))
    )
  (f3-iter 0 1 2 n))
