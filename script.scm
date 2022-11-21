(define (even? x) (= (remainder x 2) 0))
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

(define (integral-simpsons f a b n)
	(let ((h (/ (- b a) n)))
		(define (simpsons-term k)
			(let ((y-k (f (+ a (* k h)))))
				(cond
					((or (= k n) (= k 0)) y-k)
					((even? k) (* 2 y-k))
					(else (* 4 y-k)))))
		(*
			(/ h 3.0)
			(sum simpsons-term 0 1+ n))))


(integral-simpsons (lambda (x) x) 0 1 100)