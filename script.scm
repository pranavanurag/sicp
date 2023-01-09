(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define i1 (make-interval 1.6 2.4))
(define i2 (make-interval 1.4 2.4))

(define i3 (sub-interval i1 i2))
(lower-bound i3)
(upper-bound i3)
(width i3)
(width i1)
(width i2)