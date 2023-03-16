(define (make-frame-1 origin edge1 edge2) (list origin edge1 edge2))
(define (make-frame-2 origin edge1 edge2) (cons origin (cons edge1 edge2)))


(define (origin-frame-1 frame) (first frame))
(define (edge1-frame-1 frame) (second frame))
(define (edge2-frame-1 frame) (third frame))


(define (origin-frame-2 frame) (car frame))
(define (edge1-frame-2 frame) (cadr frame))
(define (edge2-frame-2 frame) (cddr frame))


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



