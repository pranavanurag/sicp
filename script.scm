;https://github.com/klutometis/sicp/blob/master/rand-update.scm
(define modulus (make-parameter (expt 2 64)))
(define multiplier (make-parameter 6364136223846793005))
(define increment (make-parameter 1442695040888963407))

(define (rand-update x)
  (modulo (+ (* (multiplier) x) (increment)) (modulus)))


(define x1 (rand-update 1))
(define x2 (rand-update x1))


(newline) (display x1)
(newline) (display x2)
