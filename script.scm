(define (make-interval a b)
  (if (> a b)
    (error "bro lmao")
    (cons a b)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c p)
  (make-center-width c (* p c)))

(define (percent i)
  (/ (width i) (center i)))

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

(define (mul-interval-center-percent x y)
  (make-center-percent
    (* (center x) (center y))
    (+ (percent x) (percent y))))

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

(define i1 (make-center-percent 12 0.001))  ;0.1%
(define i2 (make-center-percent 15 0.005))  ;0.5%

(define mul1 (mul-interval i1 i2))
(define mul2 (mul-interval-center-percent i1 i2))

(lower-bound mul1)
(upper-bound mul1)

(lower-bound mul2)
(upper-bound mul2)