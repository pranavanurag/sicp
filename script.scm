(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))

(define (iterative-improve verify-guess improve-guess)
  (lambda (first-guess)
    (define (try-guess guess)
      (if (verify-guess guess)
        guess
        (try-guess (improve-guess guess))))
    (try-guess first-guess)))

;(define 4-generator
;  (iterative-improve
;    (lambda (x)
;      (define tolerance 0.01)
;      (> tolerance (abs (- x 4))))
;    (lambda (x) 4.0)))
;
;(4-generator 3)

(define (sqrt x)
  (iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.001))
    (lambda (guess) (average guess (/ x guess)))))

((sqrt 4) 1.0)