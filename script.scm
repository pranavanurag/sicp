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

;; x = 1 + 1/x, if x is the fixed-point
;; x^2 = x + 1
;; the roots of this eqn are phi and psi
(define (golden-ratio)
    (fixed-point (lambda (y) (+ 1 (/ 1 y)))
    1.0))

(golden-ratio)