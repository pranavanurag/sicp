(define (sq x) (* x x))

(define (product term a next b)
  (if (> a b) 1
    (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product
    (lambda (x) x) 1 1+ n))

(define (product-iterative term a next b)
  (define product-iter current result) (* (term current) (product-iter (next current) result))
  (product-iter a 1))

;; wallis product is rewritten as (/ pi 2) = (* (/ 2 1) (/ 2 3) (/ 4 3) (/ 4 5) (/ 6 5) (/ 6 7)..)
;; (* (/ 4 3) (/ 16 15) (/ 36 35))
;; this is used to create the parameter 'term'
(define (pi-product n)
  (product (lambda (x) (/
        (* 4.0 (sq x))
        (- (* 4 (sq x)) 1)))
    1
    1+
    n))


(* 2.0 (pi-product 1000))