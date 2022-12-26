(define (average x y)
  (/ (+ x y) 2.0))

(define (iterative-improve verify-guess improve-guess)
  (lambda (first-guess)
    (define (try-guess guess)
      (if (verify-guess guess)
        guess
        (try-guess (improve-guess guess))))
    (try-guess first-guess)))

(define (close-enough? x y)
  (define tolerance 0.001)
  (< (abs (- x y)) tolerance))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (close-enough? (square guess) x))
    (lambda (guess) (average guess (/ x guess))))
  1.0))



(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (y-distance p1 p2)
  (- (y-point p1) (y-point p2)))
(define (x-distance p1 p2)
  (- (x-point p1) (x-point p2)))
(define (distance p1 p2)
  (sqrt (+
    (square (x-distance p1 p2))
    (square (y-distance p1 p2)))))

(distance
  (make-point 0 0)
  (make-point 3 4))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (print-segment s)
  (newline)
  (display "<")
  (print-point (start-segment s))
  (display ",")
  (print-point (end-segment s))
  (display ">"))
(define (midpoint-segment s)
  (make-point
    (average (x-point (start-segment s)) (x-point (end-segment s)))
    (average (y-point (start-segment s)) (y-point (end-segment s)))))
(define (length-segment s)
  (distance (start-segment s) (end-segment s)))

(define (make-rectangle height breadth)
  (cons height breadth))
(define (height-rectangle r) (car r))
(define (breadth-rectangle r) (cdr r))
(define (area-rectangle r)
  (* (height-rectangle r) (breadth-rectangle r)))
(define (perimeter-rectangle r)
  (* 2 (+ (height-rectangle r) (breadth-rectangle r))))

(define rectangle1 (make-rectangle 12 13))

(area-rectangle rectangle1)
(perimeter-rectangle rectangle1)