; (define (iterative-sum term a next b)
;   (define (iter x result)
;     (if (> x b)
;       result
;       (iter (next x) (+ result (term x)))))
;   (iter a 0))

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iterative combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
      result
      (iter (next i) (combiner result (term i)))))
  (iter a null-value))
  
(define (sum term a next b)
  (accumulate-iterative + 0 term a next b))

(define (product term a next b)
  (accumulate-iterative * 1 term a next b))

(sum (lambda (x) x) 1 1+ 100)
(sum (lambda (x) (* x x)) 1 1+ 10)