(define (LCG a c m)
  (lambda (x)
    (modulo (+ (* a x) c) m)))

; Define the LCG parameters
(define a 1664525)
(define c 1013904223)
(define m (expt 2 32))

; Define random-init and rand-update functions
(define random-init 42) ; Seed value
(define rand-update (LCG a c m))

; Define the rand procedure
(define rand (let ((x random-init))
  (lambda ()
    (set! x (rand-update x))
    x)))

; Usage example
(display (rand)) (newline) ; Print a random number
(display (rand)) (newline) ; Print another random number