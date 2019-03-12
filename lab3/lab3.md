Exercise 1
(lambda (x y z) (x y z)) works
(lambda () 10) works
(lambda (x) x) works
(lambda (x y) x) works

Exercise 2
((lambda (x y) (+ x (* x y))) 1 2) returns 3

((lambda (x y) 
  (+ x
    ((lambda (z) (+ (* 3 z)(/ 1 z))) (* y y))
    ))
1 2) returns 13.25

Exercise 3
> (define (square x) (* x x))
// procedure created
> square
// procedure
> (square 5)
// 25
> (define sq (lambda (x) (* x x)))
// created procedure
> sq
// procedure
> (sq 5)
// 25

> (define (make-adder num)
    (lambda (x) (+ x num)))
// procedure created
> make-adder
// procedure
> (make-adder 3)
// procedure
> ((make-adder 3) 7)
// 10

> (define plus3 (make-adder 3))
// procedure created
> plus3
// procedure
> (plus3 7)
// 10
