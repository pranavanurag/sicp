(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (fixed-point f first-guess)
    (define tolerance 0.001)
    (define (close-enough? v1 v2)
        (< (abs (- v2 v1)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            ; (newline) (display "try: guess = ") (display guess) (display ", next = ") (display next)
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
        1.0))

(sqrt 100)


(define (derivative f)
    (define dx 0.001)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

((derivative square) 10)