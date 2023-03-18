 #lang sicp 
 (#%require sicp-pict)

(define (diamond->painter)
  (segments->painter
    (list
      (segment (vect 0.5 0) (vect 1 0.5))
      (segment (vect 1 0.5) (vect 0.5 1))
      (segment (vect 0.5 1) (vect 0 0.5))
      (segment (vect 0 0.5) (vect 0.5 0)))))


(define (wave->painter)
  (segments->painter
    (let
      ((a (vect 0.2 0))
        (b (vect 0.4 0.6))
        (c (vect 0.3 0.5))
        (d (vect 0 0.7))
        (e (vect 0 0.8))
        (f (vect 0.3 0.6))
        (g (vect 0.4 0.7))
        (h (vect 0.3 0.85))
        (i (vect 0.4 1))
        (j (vect 0.6 1))
        (k (vect 0.7 0.85))
        (l (vect 0.6 0.7))
        (m (vect 0.8 0.8))
        (n (vect 1 0.6))
        (o (vect 1 0.5))
        (missed (vect 0.8 0.7))
        (p (vect 0.6 0.6))
        (q (vect 0.8 0))
        (r (vect 0.7 0))
        (s (vect 0.5 0.4))
        (t (vect 0.3 0)))
      (list
        (segment a b)
        (segment b c)
        (segment c d)
        (segment e f)
        (segment f g)
        (segment g h)
        (segment h i)
        (segment j k)
        (segment k l)
        (segment l m)
        (segment m n)
        (segment o missed)
        (segment missed p)
        (segment p q)
        (segment r s)
        (segment s t)))))

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (right-split painter n) (if (= n 0)
painter
(let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n) (if (= n 0)
painter
(let ((up (up-split painter (- n 1)))
(right (right-split painter (- n 1))))
(let ((top-left (beside up up))
(bottom-right (below right right)) (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))



(define (square-limit painter n)
(let ((quarter (corner-split painter n)))
(let ((half (beside (flip-horiz quarter) quarter))) (below (flip-vert half) half))))


(paint (square-limit (wave->painter) 5)  #:height 1000 #:width 1000)


(define (outline->painter)
  (segments->painter
    (list 
      (segment (vect 0 0) (vect 0 1))
      (segment (vect 0 0) (vect 1 0))
      (segment (vect 1 0) (vect 1 1))
      (segment (vect 0 1) (vect 1 1)))))

(define (X->painter)
  (segments->painter
    (list
      (segment (vect 0 0) (vect 1 1))
      (segment (vect 0 1) (vect 1 0)))))



;(paint (square-limit (diamond->painter) 5)  #:height 1000 #:width 1000)
