(define (average x y z) (/ (+ x (+ y z)) 3))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (define (iter i)
    (if (= i 1)
      f
      (compose f (iter (- i 1)))))
  (iter times))

(define (smoothen f)
  (define dx 0.001)
  (lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))))

(define (n-fold-smooth f n)
  ((repeated smoothen n) f))

((n-fold-smooth log 3) 12)