(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons
      (accumulate op init (map (lambda (x) (car x)) seqs))
      (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3 4) (list 1 2 3 4))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

;(matrix-*-vector (list (list 1 2 3 4) (list 5 6 7 8)) (list 1 2 3 4))

(define (transpose m)
 (accumulate-n cons '() m))

;(transpose (list (list 1 2 3 4) (list 5 6 7 8)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))

(matrix-*-matrix (list (list 1 2 3 4) (list 5 6 7 8)) (list (list 1 1) (list 1 1) (list 1 1) (list 1 1)))

(define a (list (list 34 3 -9 5) (list 12 -2 6 1)))
(define b (list (list 1 2) (list 3 -4) (list 2 5) (list -3 -17)))

(matrix-*-matrix a b)