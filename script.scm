(cons 1 (cons 2 (cons 3 (cons 4 '()))))

(define one-through-four (list 1 2 3 4))
(define (last-pair some-list)
  (define (last-pair-iter a)
    ;(newline) (display a) (display " ") (display (car a)) (display " ") (display (cdr a))
    (if (null? (cdr a))
      a
      (last-pair-iter (cdr a))))
  (last-pair-iter some-list))


(last-pair (list 23 72 149 34))
