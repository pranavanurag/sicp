(define (accumulator val)
  (lambda (augend)
    (begin (set! val (+ val augend)) val)))

(define A1 (accumulator 10))
(A1 12)