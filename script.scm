(define (sq x) (* x x))
(define (even n) (= 0 (remainder n 2)))

(define (expmod base power mod)
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


(define (miller-rabin-witness? n a)
  (let ((result (expmod a (- n 1) n)))
    (if (= result 0)
      #t
      (not (= result 1)))))

(define (random-till n) (+ 1 (random (- n 1))))

(define (miller-rabin-prime-test n times)
  (if (= times 0)
    #t
    (if (miller-rabin-witness? n (random-till n))
      #f
      (miller-rabin-prime-test n (- times 1)))))

(define (prime? n)
  (if (= n 1)
    #f
    (miller-rabin-prime-test n 100)))

;; filter? i dont even know her!
(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b) null-value
    (if (filter a)
      (combiner
        (term a)
        (filtered-accumulate combiner filter null-value term (next a) next b))
      (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (sum-of-sqd-primes a b)
  (filtered-accumulate + prime? 0 (lambda (x) (* x x)) a 1+ b))

(sum-of-sqd-primes 1 10)


(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))


(define (product-relative-primes n)
  (filtered-accumulate
    *
    (lambda (x) (= (gcd x n) 1))
    1
    (lambda (x) x)
    1
    1+
    n))

(product-relative-primes 10)