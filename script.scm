(define (LCG a c m)
  (lambda (x)
    (modulo (+ (* a x) c) m)))

(define a 1664525)
(define c 1013904223)
(define m (expt 2 32))

(define random-init 41232) ; Seed value
(define rand-update (LCG a c m))

(define (estimate-pi trials) (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x))) (let ((x2 (rand-update x1)))
      (cond ((= trials-remaining 0) (/ trials-passed trials))
            ((= (gcd x1 x2) 1)
              (iter (- trials-remaining 1) (+ trials-passed 1) x2))
            (else
              (iter (- trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

(estimate-pi 1000000)