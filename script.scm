(define (fringe tree)
  (define (fringe-iter subtree)
    (cond
      ((null? subtree) '())
      ((not (pair? subtree)) (list subtree))
      (else (append (fringe (car tree)) (fringe (cdr tree))))))
  (fringe-iter tree))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x (list 10 11 (list 12 13)) x))