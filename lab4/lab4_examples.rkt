#lang racket

;; SYSC 3101 A Winter 2019 Lab 4

;; Exercise 1

(define (make-upcounter counter)
  (lambda () 
    (set! counter (+ counter 1))
    counter))

(define counter1 (make-upcounter 0))
(counter1)
(counter1)

((make-upcounter 0))
((make-upcounter 0))

(define counter2 (make-upcounter 10))
(counter1)
(counter2)
(counter1)
(counter2)



;; Exercise 2 

(define (make-counter counter)
  
  (define (count-up) 
    (set! counter (+ counter 1))
    counter)
  
  (define (count-down)
    (if (> counter 0)
        (begin (set! counter (- counter 1))
               counter)
        "Counter is 0"))

  (define (dispatch cmd)
    (cond ((eq? cmd 'inc) count-up)
          ((eq? cmd 'dec) count-down)
          (else (error "Unknown command:" cmd))))
  
  dispatch)

(define counter3 (make-counter 0))
counter3

(counter3 'inc)
(counter3 'dec)
;; (counter3 'reset)



(counter3 'inc)
(counter3 'inc)
(counter3 'dec)
(counter3 'dec)
(counter3 'dec)
;; (counter3 'reset)

;; Exercise 3

(define (make-counter-with-let initial-count)

  (let ((counter initial-count))
 
    (define (count-up)
      (set! counter (+ counter 1))
      counter)
 
    (define (count-down)
      (if (> counter 0)
          (begin (set! counter (- counter 1))
                 counter)
          "Counter is 0"))

    (define (dispatch cmd)
      (cond ((eq? cmd 'inc) count-up)
            ((eq? cmd 'dec) count-down)
            (else (error "Unknown command:" cmd))))
 
    dispatch))
