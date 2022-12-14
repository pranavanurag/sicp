(define (cont-frac n d k)
    (define (cont-frac-iter i ans)
        (if (< i 1)
            ans
            (cont-frac-iter
                (- i 1)
                (/ (n i) (+ (d i) ans)))))
    (cont-frac-iter k 0))

(define (golden-ratio k)
    (/
        1
        (cont-frac
            (lambda (x) 1.0)
            (lambda (x) 1.0)
            k)))

(golden-ratio 12)