(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define (get key-1 key-2)
  ((operation-table 'lookup-proc) key-1 key-2))

(define (put key-1 key-2 value)
  ((operation-table 'insert-proc!) key-1 key-2 value))


;; functions above this comment are assumed and not a part of current exercise


(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2)))) 
  
(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))
  
(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

;; the above functions work with any representation as long as they expose
;; real-part, imag-part, magnitude, angle

;; we may use type tags to sign the data objects
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;; real and imaginary parts representation
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z) (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))
(define (angle-rectangular z) (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) (make-from-real-img-rectangular (* r (cos a)) (* r (sin a))))


;; magnitude and angle representation
(define (real-part-polar z) (* (cos (angle-polar z)) (magnitude-polar z)))
(define (imag-part-polar z) (* (sin (angle-polar z)) (magnitude-polar z)))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (make-from-mag-ang-polar (sqrt (+ (square x) (square y))) (atan y x)))
(define (make-from-mag-ang-polar m a) (attach-tag 'polar (cons m a)))


(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (real-part z)
  (cond
    ((rectangular? z) (real-part-rectangular (contents z)))
    ((polar? z) (real-part-polar (contents z)))
    (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond
    ((rectangular? z) (imag-part-rectangular (contents z)))
    ((polar? z) (imag-part-polar (contents z)))
    (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond
    ((rectangular? z) (magnitude-rectangular (contents z)))
    ((polar? z) (magnitude-polar (contents z)))
    (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond
    ((rectangular? z) (angle-rectangular (contents z)))
    ((polar? z) (angle-polar (contents z)))
    (else (error "Unknown type: ANGLE" z))))


(define polar-example (make-from-real-imag-polar 12 13))
(real-part polar-example)
(imag-part polar-example)
(magnitude polar-example)
(angle polar-example)


(define rectangular-example (make-from-real-imag-rectangular 12 13))
(real-part rectangular-example)
(imag-part rectangular-example)
(magnitude rectangular-example)
(angle rectangular-example)