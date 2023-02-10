(define (square-list items)
  (if (null? items)
    '()
    (cons
      (square (car items))
      (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

(square-list (list 1 2 34 4 5 5))