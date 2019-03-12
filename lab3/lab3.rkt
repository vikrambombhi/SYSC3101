#lang racket

(define (build-naturals n)
  (build-list n (lambda (x) x))
  )

(define (build-rationals n)
  (build-list n (lambda (x) (/ 1 (+ 1 x))))
  )

(define (build-evens n)
  (build-list n (lambda (x) (* 2 x)))
  )

(build-naturals 5)
(build-rationals 5)
(build-evens 5)

(define (cubic a b c) (lambda (x) 
                        (+ (expt x 3) (* a (expt x 2)) (* b x) c)
                        ))

((cubic 1 2 3) 4)

(define (twice f) (lambda (n)
                     (f (f n))
                     ))

(define (square x) (* x x))
((twice square) 5)
