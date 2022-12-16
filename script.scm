(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (define (iter i)
    (if (= i 1)
      f
      (compose f (iter (- i 1)))))
  (iter times))

((repeated square 2) 5)