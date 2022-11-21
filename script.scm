(define (sq x) (* x x))
(define (cube x) (* (sq x) x))
(define (incr x) (+ 1 x))

(define (sum term a next b)
	(if (> a b)
		0
		(+
			(term a)
			(sum term (next a) next b))))

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(*
		(sum f (+ a (/ dx 2.0)) add-dx b)
		dx))

;; (define (integral-simpsons))


(integral cube 0 1 0.001)