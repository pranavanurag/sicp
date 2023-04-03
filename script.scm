(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((> (car set) x) (cons x set))
    ((= (car set) x) set)
    (else (cons (car set) (adjoin-set x (cdr set))))))

(define myset (list 1 2 3 4 5))
(adjoin-set 2.5 myset)

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
        ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
        ((< x1 x2) (intersection-set (cdr set1) set2))
        ((< x2 x1) (intersection-set set1 (cdr set2)))))))


(define myset2 (list 4 5 6))


(intersection-set myset (adjoin-set 1 myset2))


(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x1 (car set1)) (x2 (car set2)))
       (cond
        ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
        ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
        ((< x1 x2) (cons x1 (union-set (cdr set1) set2))))))))

(union-set (list 1 2.5 3 4) (list 2 3 4 7))