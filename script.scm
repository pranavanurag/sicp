(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define i1 (make-interval 1.3 1.4))
(define i2 (make-interval 1.5 2.4))

(lower-bound (sub-interval i1 i2))
(upper-bound (sub-interval i1 i2))