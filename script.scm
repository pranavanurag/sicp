(define (double f)
    (lambda (x) (f (f x))))

((double 1+) 2)

(((double (double double)) 1+) 5)