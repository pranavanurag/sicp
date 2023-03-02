(define (sq x) (* x x))
(define (even n) (= 0 (remainder n 2)))

(define (fast-exp-mod base power mod)
  (if (= power 0)
    (remainder 1 mod)
    (if (even power)
      (fast-exp-mod (remainder (sq base) mod) (/ power 2) mod)
      (remainder (* base (fast-exp-mod (sq base) (/ (- power 1) 2) mod)) mod))))

(define (fermat-test n)
  (define (try-it a) (= (fast-exp-mod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (prime? n) (fast-prime? n 50))






(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
          (cons
            (car sequence)
            (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
        (map
          (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (flatmap proc seq) (accumulate append '() (map proc seq)))

(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 100)