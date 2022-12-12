(define (average x y) (/ (+ x y) 2))
(define tolerance 0.001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v2 v1)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (newline) (display "try: guess = ") (display guess) (display ", next = ") (display next)
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(define x-to-the-x-is-a-thousand
    (fixed-point
        (lambda (x) (/ (log 1000) (log x)))
        10.0))

(define x-to-the-x-is-a-thousand-avg-damp
    (fixed-point
        (lambda (x) (average x (/ (log 1000) (log x))))
        10.0))