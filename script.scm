(define (sq x) (* x x))

(define (even n) (= 0 (remainder n 2)))

(define (expmod base power mod)
	;; (display "\texpmod ") (display base) (display " ") (display power) (display " ") (display mod) (newline)
	(if (= power 0)
		1
		(if (even power)
			(let ((potential-non-trivial-sqrt (expmod base (/ power 2) mod)))
				(let ((the-square (sq potential-non-trivial-sqrt)))
					(if (and
							(not (= -1 potential-non-trivial-sqrt))
							(and
								(not (= 1 potential-non-trivial-sqrt))
								(= the-square 1)))
						0
						(remainder the-square mod))))
			(remainder (* base (expmod base (- power 1) mod)) mod))))


(expmod 7 9 19)

;; returns true if a is
(define (miller-rabin-witness? n a)
	;; (display "miller-rabin-witness asked if ") (display a) (display " is a witness of ") (display n) (display ": ") (display "expmod") (display " ") (display a) (display " ") (display (- n 1)) (display " ") (display n) (display " will be called") (newline)
	(let ((result (expmod a (- n 1) n)))
		;; (display "\t") (display result) (newline)
		(if (= result 0)
			#t
			(not (= result 1)))))

(define (random-till n) (+ 1 (random (- n 1))))

(define (miller-rabin-prime-test n times)
	;; (newline) (display "miller-rabin-prime-test ") (display n) (display " ") (display times) (newline)
	(if (= times 0)
		#t
		(if (miller-rabin-witness? n (random-till n))
			#f
			(miller-rabin-prime-test n (- times 1)))))

(define (miller-rabin-prime? n)
	(miller-rabin-prime-test n 100))

(miller-rabin-prime? 11)
(miller-rabin-prime? 12)
(miller-rabin-prime? 13)
(miller-rabin-prime? 14)
(miller-rabin-prime? 15)
(miller-rabin-prime? 16)
(miller-rabin-prime? 17)
(miller-rabin-prime? 18)
(miller-rabin-prime? 19)
(miller-rabin-prime? 47)
(miller-rabin-prime? 42)
(miller-rabin-prime? 32)
(miller-rabin-prime? 4)