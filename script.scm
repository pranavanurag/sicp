(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (fixed-point f first-guess)
    (define tolerance 0.001)
    (define (close-enough? v1 v2)
        (< (abs (- v2 v1)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            ; (newline) (display "try: guess = ") (display guess) (display ", next = ") (display next)
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(define (fixed-point-of-transform f transform guess)
    (fixed-point (transform f) guess))

(define (derivative f)
    (define dx 0.001)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newtons-transform f)
    (lambda (x)(- x (/ (f x) ((derivative f) x)))))

(define (newtons-method f first-guess)
    (fixed-point-of-transform f newtons-transform first-guess))

(newtons-method (lambda (x) (- x 1)) 12)