(define (cont-frac n d k)
    (define (cont-frac-iter i ans)
        (if (< i 1)
            ans
            (cont-frac-iter
                (- i 1)
                (/ (n i) (+ (d i) ans)))))
    (cont-frac-iter k 0))

(define (e-minus-2 k)
    (cont-frac
        (lambda (x) 1.0)
        (lambda (x)
            (if (= 2 (remainder x 3))
                (* (/ 2.0 3) (+ x 1))
                1.0))
        k))

(define (e-approximation k)
    (+ 2 (e-minus-2 k)))

(e-approximation 20)