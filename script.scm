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

(define myp (make-point 12 13))
(print-point myp)

(define mys (make-segment (make-point 12 13) (make-point 13 15)))
(print-segment mys)

(define (average x y)
  (/ (+ x y) 2.0))

(define (midpoint-segment s)
  (make-point
    (average (x-point (start-segment s)) (x-point (end-segment s)))
    (average (y-point (start-segment s)) (y-point (end-segment s)))))

(print-point (midpoint-segment mys))