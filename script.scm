(define (sq x) (* x x))

(define (even n) (= 0 (remainder n 2)))

(define (expmod base power mod)
	(if (= power 0)
		1
		(if (even power)
			(let ((potential-non-trivial-sqrt (expmod base (/ power 2) mod)))
				(let ((the-square (sq potential-non-trivial-sqrt)))
					(remainder the-square mod)))
			(remainder (* base (expmod base (- 1 power) mod)) mod))))
