(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (define (iter i)
    (if (= i 1)
      f
      (compose f (iter (- i 1)))))
  (iter times))

(define (fixed-point f first-guess)
  (define tolerance 0.01)
  (define (close-enough? v1 v2)
    (< (abs (- v2 v1)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline) (display "try: guess = ") (display guess) (display ", next = ") (display next)
      (if (close-enough? guess next)
        guess
        (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (sqrt n)
  (fixed-point
    (average-damp (lambda (x) (/ n x)))
    2.0))


(define (power x y) ;; only works for positive y
  ((repeated
    (lambda (i) (* i x))
    y)
  1))

(define (nth-root x n damps)
  (fixed-point
    ((repeated average-damp damps) (lambda (y) (/ (power x (- n 1)) y)))
    2.0))

(nth-root 16 2 1)