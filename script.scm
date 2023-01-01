(define (divisibility a b)
  (define (iter x)
    (if (or (> (remainder x b) 0) (= x 0))
      0
      (+ 1 (iter (/ x b)))))
  (iter a))

(divisibility 108 3)

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car p)
  (divisibility p 2))

(define (cdr p)
  (divisibility p 3))


(define mypair (cons 14 45))
(car mypair)
(cdr mypair)