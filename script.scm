(define (filter predicate sequence)
    (cond
        (null? sequence) '()
        ((predicate (car sequence))
            (cons
                (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter (lambda (x) (> x 0)) (list 1 -1 0 12 13 -100))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (accumulate
            op
            (op initial (car sequence))
            (cdr sequence))))

(accumulate + 0 (list 1 2 34 5))


(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 0 9)




