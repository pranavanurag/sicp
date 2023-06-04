(define modulus (make-parameter (expt 2 64)))
(define multiplier (make-parameter 6364136223846793005))
(define increment (make-parameter 1442695040888963407))

; this rand-update actually sucks, but for this exercise it doesn't really matter 
(define (rand-update x)
  (modulo (+ (* (multiplier) x) (increment)) (modulus)))

(define rand
  (let ((x 42))
    (lambda (mode)
      (define generator (begin (set! x (rand-update x)) x))
      (define (init new-init) (set! x new-init))

      (cond
        ((eq? mode 'generate) generator)
        ((eq? mode 'reset) init)
        (else (error "invalid mode" mode))))))

  
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 12)
(rand 'generate)