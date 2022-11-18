(define (sq x) (* x x))

(define (even n) (= 0 (remainder n 2)))

(define (fast-exp-mod base power mod)
  (if (= power 0)
	  (remainder 1 mod)
	  (if (even power)
		  (fast-exp-mod (remainder (sq base) mod) (/ power 2) mod)
		  (remainder (* base (fast-exp-mod base (- power 1) mod)) mod))))

(define (fermat-test n i)
	(let ((themod (fast-exp-mod i n n)))
		;; (newline )(display "fermat-test ") (display n) (display " ") (display i) (display (= themod i))
		(= themod i)))

(define (fermat-test-with-random n)
	(fermat-test n (+ 1 (random (- n 1)))))

(define (test-prime n times)
	(if (= times 0) #t
		(if (fermat-test-with-random n) (test-prime n (- times 1))
			#f)))
	
(define (test-prime-default n) (test-prime n 1000))

(define (test-carmichael n)
	(define (test-carmichael-iter n i)
		(if (= i n) #t
		(if (fermat-test n i) (test-carmichael-iter n (+ 1 i))
			#f)))
	(test-carmichael-iter n 1))



(test-prime-default 123)
(test-prime-default 23456)
(test-prime-default 1323532467787872)
(test-prime-default 12)
(test-prime-default 79)
(test-prime-default 81)
(test-prime-default 56)

(test-prime-default 561)
(test-prime-default 1105)
(test-prime-default 1729)
(test-prime-default 2465)
(test-prime-default 2821)
(test-prime-default 6601)