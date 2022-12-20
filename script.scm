(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (define (iter i)
    (if (= i 1)
      f
      (compose f (iter (- i 1)))))
  (iter times))

(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v2 v1)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline) (display "try: guess = ") (display guess) (display ", next = ") (display next)
      (if (close-enough? guess next)
        guess
        (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2.0))

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (power x y)
  (if (= y 0)
    1.0
    ((repeated (lambda (i) (* i x)) y) 1.0)))

(define (nth-root x n damps)
  (fixed-point
    ((repeated average-damp damps) (lambda (y) (/ x (power y (- n 1)))))
    1.5))

;(nth-root 16 1 1)
;(nth-root 16 2 1)
;(nth-root 16 3 1)
;(nth-root 16 4 2)
;(nth-root 16 5 2)
;(nth-root 16 6 2)
;(nth-root 16 7 2)
;(nth-root 16 8 3)
;(nth-root 16 9 3)
;(nth-root 16 10 3)
;(nth-root 16 12 3)
;(nth-root 16 14 3)
;(nth-root 16 15 3)
;(nth-root 16 16 4)
