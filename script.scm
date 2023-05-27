(define (LCG a c m)
  (lambda (x)
    (modulo (+ (* a x) c) m)))

(define a 1664525)
(define c 1013904223)
(define m (expt 2 32))

(define random-init 41232) ; Seed value
(define rand-update (LCG a c m))

(define rand (let ((x random-init)) (lambda () (set! x (rand-update x)) x)))

(define (estimate-pi trials) (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test) (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))


(estimate-pi 10000)