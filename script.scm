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



;transform-painter takes a painter and the description of a new frame
;(origin, corner1, corner2 -> question - why not use the frame abstraction?)
;and returns a painter 
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
          new-origin
          (sub-vect (m corner1) new-origin)
          (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)))

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))

(define (rotate-180-cc painter)
  (transform-painter
    painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)))

(define (rotate-270-cc painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))
    