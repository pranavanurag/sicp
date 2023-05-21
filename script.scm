x2 = (rand-update 1)
x3 = (rand-update x2)

(define rand
  (let ((x random-int))
    (lambda () (set! x (rand-update x)) x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carle trials cesaro-test))))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (-trials-remaining 1) (+ trials-passed 1)));HAHA GOOD LUCK DEFINING AN EXPERIMENT WITH NO ARGUMENTS
      (else (iter trials-remaining trials-passed))))
    (iter trials))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-int))))
    ;it's annoying to carry this random-int in any kind of monte carlo simulation i want to do
  ;this should be a detail in the black box "generating random numbers", not leak out


(define (random-gcd-test trials random-int)
  (define iter trials-remaining trials passed x)
    ())
