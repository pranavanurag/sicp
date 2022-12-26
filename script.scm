(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
    
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+
      (* (numer x) (denom y))
      (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (equal-rat? x y)
(= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))


(define one-third (make-rat 1 3))

(define sum (add-rat one-third one-third))
(print-rat sum)
