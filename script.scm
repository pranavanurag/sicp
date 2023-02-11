(define (for-each f items)
  ;(newline) (display "for-each invoked: ") (display items)
  (if (null? items)
    #t
    ((for-each f (cdr items)) (f (car items)))))


(for-each
  (lambda (x) (newline) (display x))
  (list 57 321 88))