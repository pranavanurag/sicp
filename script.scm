(define (random-in-range low high)
  (let ((range (- high low)))
    (lambda () (+ low (random range)))))

(define (unit-circle-predicate x-generator y-generator)
  (let ((x (x-generator)) (y (y-generator)))
    (<
      (+ (square (- x 0.5)) (square (- y 0.5)))
      (square 0.5))))
  
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define rect-area (* (- x2 x1) (- y2 y1)))
  (define (integral-experiment)
    (predicate (random-in-range x1 x2) (random-in-range y1 y2)))
  (/ (monte-carlo trials integral-experiment) rect-area))


(define (estimate-pi trials)
  (estimate-integral
    unit-circle-predicate
    0.0 1.0 0.0 1.0
    100000))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(estimate-pi 10000)
