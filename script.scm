(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))


(define (div-interval x y)
  (if (= (width y) 0)
    (error "width of interval y is 0")
    (mul-interval
      x
      (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
    (p2 (* (lower-bound x) (upper-bound y)))
    (p3 (* (upper-bound x) (lower-bound y)))
    (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define i1 (make-interval 1.6 2.4))
(define i2 (make-interval 1.4 1.4))

(div-interval i1 i2)
