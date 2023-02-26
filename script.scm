(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define x (list 1 2 3))
(define y (list 4 5 6))
(accumulate cons y x)

