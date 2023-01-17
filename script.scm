(define (make-interval a b)
  (if (> a b)
    (error "bro lmao")
    (cons a b)))

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
  (let
    ((l1 (lower-bound x))
      (u1 (upper-bound x))
      (l2 (lower-bound y))
      (u2 (upper-bound y)))
    (if (> l1 0)
      (if (> l2 0)
        (make-interval (* l1 l2) (* u1 u2)) ; l1 > 0, l2 > 0, u1 > 0, u2 > 0
        (if (> u2 0)
          (make-interval (* u1 l2) (* u1 u2))  ; l1 > 0, u1 > 0, l2 < 0, u2 > 0
          (make-interval (* u1 l2) (* l1 u2)))) ; l1 > 0, u1 > 0, l2 < 0, u2 < 0
      (if (> l2 0)
        (if (> u1 0)
          (make-interval (* l1 u2) (* u1 u2))  ; l1 < 0, u1 > 0, l2 > 0, u2 > 0
          (make-interval (* l1 u2) (* u1 l2))) ; l1 < 0, u1 < 0, l2 > 0, u2 > 0
        (if (> u1 0)
          (if (> u2 0)
            (make-interval (min (* u1 l2) (* l1 u2)) (max (* l1 l2) (* u1 u2)))  ; l1 < 0, u1 > 0, l2 < 0, u2 > 0
            (make-interval (* u1 l2) (* l1 l2))) ; l1 < 0, u1 > 0, l2 < 0, u2 < 0
          (if (> u2 0)
            (make-interval (* l1 u2) (* l1 l2))  ; l1 < 0, u1 < 0, l2 < 0, u2 > 0
            (make-interval (* u1 u2) (* l1 l2)))))))); l1 < 0, u1 < 0, l2 < 0, u2 < 0

(define i1 (mul-interval (make-interval 2 2.5) (make-interval 3 3.5)))
(define i2 (mul-interval (make-interval 2 2.5) (make-interval -3 3.5)))
(define i3 (mul-interval (make-interval 2 2.5) (make-interval -3.5 -3)))
(define i4 (mul-interval (make-interval -2 2.5) (make-interval 3 3.5)))
(define i5 (mul-interval (make-interval -2 2.5) (make-interval -3 3.5)))
(define i6 (mul-interval (make-interval -2 2.5) (make-interval -3.5 -3)))
(define i7 (mul-interval (make-interval -2.5 -2) (make-interval 3 3.5)))
(define i8 (mul-interval (make-interval -2.5 -2) (make-interval -3 3.5)))
(define i9 (mul-interval (make-interval -2.5 -2) (make-interval -3.5 -3)))

(newline) (display (lower-bound i1)) (display ",") (display (upper-bound i1))
(newline) (display (lower-bound i2)) (display ",") (display (upper-bound i2))
(newline) (display (lower-bound i3)) (display ",") (display (upper-bound i3))
(newline) (display (lower-bound i4)) (display ",") (display (upper-bound i4))
(newline) (display (lower-bound i5)) (display ",") (display (upper-bound i5))
(newline) (display (lower-bound i6)) (display ",") (display (upper-bound i6))
(newline) (display (lower-bound i7)) (display ",") (display (upper-bound i7))
(newline) (display (lower-bound i8)) (display ",") (display (upper-bound i8))
(newline) (display (lower-bound i9)) (display ",") (display (upper-bound i9))
