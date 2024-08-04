(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond
            ((eq? op 'real-part) x)
            ((eq? op 'imag-part) y)
            ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
            ((eq? op 'angle) (atan y x))
            (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
    dispatch)

(define (apply-generic op arg) (arg op))


(define mynum (make-from-real-imag 12 13))
(apply-generic 'magnitude mynum)


(define (make-from-mag-ang m a)
    (define (dispatch op)
        (cond
            ((eq? op 'real-part) (* m (cos a)))
            ((eq? op 'imag-part) (* m (sin a)))
            ((eq? op 'magnitude) m)
            ((eq? op 'angle) a)
            (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
    dispatch)

(define mynum2 (make-from-mag-ang 12 13))
(apply-generic 'magnitude mynum2)
(apply-generic 'angle mynum2)
(apply-generic 'real-part mynum2)
(apply-generic 'imag-part mynum2)