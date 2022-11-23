(define (product term a next b)
	(if (> a b) 1
		(* (term a) (product term (next a) next b))))

(define (factorial n)
	(product
		(lambda (x) x) 1 1+ n))



;; (product (lambda (x) x) 1 1+ 10)
;; (factorial 5)