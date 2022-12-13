(define (cont-frac n d k)
    (define (cont-frac-sub i)
        (if (> i k)
            0
            (/
                (n i)
                (+ (d i) (cont-frac-sub (+ i 1))))))
    (cont-frac-sub 1))

(define (golden-ratio k)
    (/
        1
        (cont-frac
            (lambda (x) 1.0)
            (lambda (x) 1.0)
            k)))

(golden-ratio 12)