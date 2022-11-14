(define (sq x) (* x x))

(define (even n) (= 0 (remainder n 2)))

(define (fast-exp-mod base power mod)
  (if (= power 0)
	  (remainder 1 mod)
	  (if (even power)
		  (fast-exp-mod (remainder (sq base) mod) (/ power 2) mod)
		  (remainder (* base (fast-exp-mod base (- power 1)) mod) mod))))



(define (fermat-test n)
	(define (try-it a) (= (fast-exp-mod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond
		((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))

(define (timed-prime-test n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	;; (newline)
	;; (display "start-prime-test ") (display n) (display " ") (display start-time)
	(if (fast-prime? n 100) (report-prime n (- (runtime) start-time))))
		
(define (report-prime n elapsed-time)
	(newline)
	(display n)
	(display " *** ")
	(display elapsed-time))

(define (search-for-primes start end)
	(timed-prime-test start)
	(if (< start end) (search-for-primes (+ 1 start) end))
	end)

(define (test-carimchel-iter n i)
	())

(define (test-carmichel n))

(search-for-primes 10000000000000 10000000001000)

(define (expmod base exp m)
    (cond
    ((= exp 0) 1)
    ((even? exp) (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))