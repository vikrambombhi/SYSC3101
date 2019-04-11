| Expression                             | Racket         | racket-1              |
|----------------------------------------|----------------|-----------------------|
| 5                                      | 5              |                       |
| *                                      | #<procedure:*> | #<procedure:*>        |
| (quote (1 2 3))                        | '(1 2 3)       | '(1 2 3)              |
| (if (= 5 (+ 2 3)) "equal" "not equal") | "equal"        | "equal"               |
| (lambda (x) (+ x 1))                   | #<procedure>   | '(lambda (x) (+ x 1)) |
| (* 2 3)                                | 6              | 6                     |
| (* 2 3 4)                              | 24             | 24                    |
| (* 2 (* 3 4))                          | 24             | 24                    |
| ((lambda (x) (+ x 1)) 5)               | 6              | 6                     |
