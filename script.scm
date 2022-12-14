(define (cont-frac n d k)
    (define (cont-frac-iter i ans)
        ; (newline) (display "cont-frac-iter, i = ") (display i) (display ", num = ") (display (n i)) (display ", den = ") (display (d i)) (display ", ans = ") (display ans)
        (if (< i 1)
            ans
            (cont-frac-iter
                (- i 1)
                (/ (n i) (+ (d i) ans)))))
    (cont-frac-iter k 0))

(define (tan-cf x k)
    (/
        (cont-frac
            (lambda (i) (* -1 (* x x)))
            (lambda (i) (- (* 2 i) 1))
            k)
        (* -1 x)))


(define pi 3.14159265359)

(tan-cf (/ pi 4) 10)