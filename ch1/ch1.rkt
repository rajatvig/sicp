#lang sicp

(#%require racket/math)
(#%require math/base)

(define pi 3.14)
(define radius 10)
(define (area radius) (* pi (* radius radius)))

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
      (< (abs (- (square guess) x)) 0.0000001))
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

(define (pascal row col)
  (cond ((or (< row col)
             (< col 1)) 0 )
        ((or (= col 1)
             (= col row)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col )))))

(define (expt-r b n)
  (if (= n 0)
      1
      (* b (expt-r b (- n 1)))))

(define (expt-i b n)
  (define (expt-iter p n)
    (if (= n 0)
        p
        (expt-iter (* p b) (- n 1)))
    )
  (expt-iter 1 n))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )

;; 1.16
(define (fast-expt-a b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt-a (square b) (/ n 2)))
        (else (* b (fast-expt-a b (- n 1))))
        )
  )

(define (fast-expt-2 b n)
  (define (fast-expt-iter p b n)
    (cond ((= n 0) p)
          ((even? n) (fast-expt-iter p (square b) (/ n 2)))
          (else (fast-expt-iter (* p b) b (- n 1)))
          )
    )
  (fast-expt-iter 1 b n)
  )

;; 1.17
(define (c-mult a b)
  (if (= b 0)
      0
      (+ a (c-mult a (- b 1))))
  )

(define (c-mult-2 a b)
  (define (c-mult-iter p a b)
    (if (= b 0)
        p
        (c-mult-iter (+ p a) a (- b 1)))
    )
  (c-mult-iter 0 a b)
  )

(define (c-mult-3 a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (c-mult-3 (double a) (halve b)))
        (else (+ a (c-mult-3 a (- b 1))))
        )
  )

(define (c-mult-4 a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (define (c-mult-iter p a b)
    (cond ((= b 0) p)
          ((even? b) (c-mult-iter p (double a) (halve b)))
          (else (c-mult-iter (+ p a) a (- b 1)))
          )
    )
  (c-mult-iter 0 a b)
  )

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor t)
    (cond ((> (square t) n) n)
          ((divides? t n) t)
          (else (find-divisor (+ t 1)))))
  (find-divisor 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-natural (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report (- (runtime) start-time))
      #f))

(define (report elapsed-time)
  (newline)
  (display " *** ")
  (display elapsed-time)
  #t)

;; 1.22
(define (search-for-primes n count)
  (cond ((= count 0))
        ((fast-prime? n 10)
         (report n)
         (search-for-primes (+ 1 n) (- count 1)))
        (else (search-for-primes (+ 1 n) count))))

(define (timed-search n count)
  (define (timed-search-inner n count start-time)
    (search-for-primes n count)
    (report (/ (- (runtime) start-time) count))
    )
  (timed-search-inner n count (runtime)))
