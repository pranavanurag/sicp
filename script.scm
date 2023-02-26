(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op
            (car sequence)
            (accumulate op initial (cdr sequence)))))

(define x (list 1 2 3))
(define y (list 4 5 6))
(define (append2 x y)
    (accumulate cons y x))

(define (map2 p sequence)
    (accumulate
        (lambda (current rest) (cons (p current) rest))
        '()
        sequence))


(map2 square (list 1 2 3 4))


(define (length sequence)
    (accumulate (lambda (current rest) (+ 1 rest)) 0 sequence))

(length (list 1 2 3 5 123 5 917 1977 20 2974))
