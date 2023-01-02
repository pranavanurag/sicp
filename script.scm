(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-numbers a)
  (lambda (b)
    (lambda (f)
      (lambda (x)
       ((a f) ((b f) x))))))

;((((add-numbers one) one) add-1) 0)
((((add-numbers one) one) add-1) 0)