(define (random-in-range low high)
  (let ((range (- high low)))
    (lambda () (+ low (random range)))))

(define (unit-circle-predicate x-generator y-generator)
  (let ((x (x-generator)) (y (y-generator)))
    (<
      (+ (square (- x 1.0)) (square (- y 1.0)))
      (square 1.0))))
  
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (define rect-area (* (- x2 x1) (- y2 y1)))
  (define (integral-experiment)
    (predicate (random-in-range x1 x2) (random-in-range y1 y2)))
  (* rect-area (monte-carlo trials integral-experiment)))


(define (estimate-pi trials)
  (estimate-integral
    unit-circle-predicate
    0.0 2.0 0.0 2.0
    trials))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(estimate-pi 100000)
