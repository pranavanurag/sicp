(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))))


(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (first frame))
(define (edge1-frame frame) (second frame))
(define (edge2-frame frame) (third frame))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect 
        (scale-vector (xcord-vect v) (edge1-frame frame))
        (scale-vector (ycord-vect v) (edge2-frame frame))))))

;painter is a procedure that takes a frame as an input 
;and draws a particular image shifted and scaled to that frame
;p is a painter and f is a frame
;we produce p's image in f by calling (p f)

;details of primitive painters depend on the graphics system and the type of image to be drawn
;assume we have a procedure draw-line that draws a line on the screen bw two pts
;then we may create paints for line drawings by drawing multiple line segments


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;segments are give using coordinates wrt unit square
;for each segment the painter transforms the segment with the frame coordinate map
;and draws a line bw these transformed end points of the segment using the primitive draw-line

;the details of the primitives do not matter! 
;(question - how do i test if my impl works without "an" implementation of primitives)
;is it possible to do this? when i use cons, car and cdr without implementing them, 
;i still am using *some* implementation of cons, car and cdr on this system



(define make-segment cons)
(define start-segment car)
(define end-segment cdr)


;beautiful - frame is not required for this procedure as an argument
;this procedure simply returns a procedure which takes a frame as input
;and draws its corners. the "corners" can be expressed as the corners of the unit sq
(define (outline->painter)
  (segments->painter
    (list 
      (make-segment (make-vect 0 0) (make-vect 0 1))
      (make-segment (make-vect 0 0) (make-vect 1 0))
      (make-segment (make-vect 1 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 1)))))


(define (X->painter)
  (segments->painter
    (list
      (make-segment (make-vect 0 0) (make-vect 1 1))
      (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define (diamond->painter)
  (segments->painter
    (list
      (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
      (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))


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



