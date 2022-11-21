(define (sq x) (* x x))
(define (cube x) (* (sq x) x))
(define (incr x) (+ 1 x))

(define (sum term a next b)
	(if (> a b)
		0
		(+
			(term a)
			(sum term (next a) next b))))

(define (sum-cubes a b)
	(sum cube a incr b))

(sum-cubes 1 10)