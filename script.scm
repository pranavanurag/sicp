(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (accumulate
            op
            (op initial (car sequence))
            (cdr sequence))))

(define (accumulate2 op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate2 op intial (cdr sequence)))))


