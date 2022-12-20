(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))

(define (iterative-improve verify-guess improve-guess)
  (lambda (first-guess)
    (define (try-guess guess)
      (if (verify-guess guess)
        guess
        (try-guess (improve-guess guess))))
    (try-guess first-guess)))

(define (close-enough? x y)
  (define tolerance 0.001)
  (< (abs (- x y)) tolerance))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (close-enough? (square guess) x))
    (lambda (guess) (average guess (/ x guess))))
  1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess) (close-enough? guess (f guess)))
    (lambda (guess) (f guess)))
  first-guess))
