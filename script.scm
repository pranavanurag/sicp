(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define x (make-rat 12 13))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(numer x)
(denom x)
(print-rat x)