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

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

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

(define (par1 r1 r2)
  (div-interval
    (mul-interval r1 r2)
    (add-interval r1 r2)))


(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval
        (div-interval one r1)
        (div-interval one r2)))))

(define a (make-center-percent 10 0.003))
(lower-bound a)
(upper-bound a)
(define div (div-interval a a))
(lower-bound div)
(upper-bound div)
(center div)
(percent div)



(define b (make-center-percent 20 0.005))

(define div2 (div-interval a b))
(center div2)
(percent div2)

(define div3 (div-interval (make-interval 1 1) (div-interval b a)))
(center div3)
(percent div3)


(define ans1 (par1 a b))
(center ans1)
(percent ans1)

(define ans2 (par2 a b))
(center ans2)
(percent ans2)