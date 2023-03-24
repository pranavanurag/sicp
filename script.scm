(define (my-equal? seq1 seq2)
  (cond
    ((and (null? seq1) (null? seq2)) #t)
    ((or (null? seq1) (null? seq2)) #f)
    ((eq? (car seq1) (car seq2)) (my-equal? (cdr seq1) (cdr seq2)))
    (else #f)))

(my-equal? (list 1 2 3) (list 1 3))