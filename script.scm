(define tolerance 0.001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v2 v1)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(fixed-point cos 1.0)