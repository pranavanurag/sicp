(define (sign x) (if (= x 0) 0 (/ x (abs x))))

(define (make-rat n d)
  (define (make-rat-sub n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (let ((fsign (* (sign n) (sign d))))
    (set! n (* fsign (abs n)))
    (set! d (abs d))
    (newline) (display "updated values of n and d ") (display n) (display ", ") (display d) (display ", fsign = ") (display fsign)
    (make-rat-sub n d)))

    
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

(define m-one-third (make-rat -1 3))

(numer m-one-third)
(denom m-one-third)

(define one-third (make-rat 1 3))

(define sum (add-rat one-third m-one-third))
(print-rat sum)
